/+
version `VADGL_DebugGLCalls`:
    prints all OpenGL calls

version `VADGL_EnableChecks`:
    This allows calling `glGetError` after every OpenGL call. This might have a
    significant performance hit which is why it's only enabled by default on debug
    versions

version `VADGL_DisableChecks`:
    Disables `glGetError` checks. This only affects debug builds since cheks
    are only enabled by default on debug builds.

- Most functions allocate because of strings for error messages
- Must functions don't throw here.
    All functions which can fail should return a Result!Type
    Exceptions are only used in a handful of functions dealing with files
- Do not overcomplicate anything
- Think about OpenGL support fucking later
+/
module vadgl.gl3;

import std.conv                 : to;
import std.format               : format;
import std.meta                 : AliasSeq, allSatisfy;

// OpenGL bindings
import bindbc.opengl;
import vadgl.types;
import vadgl.error;

import std.string               : toStringz;

import std.algorithm            : endsWith, among;

import result;


version (VADGL_DisableChecks) {
    enum bool checkGLCalls = false;
}
else version (VADGL_EnableChecks) {
    enum bool checkGLCalls = true;
}
else debug {
    enum bool checkGLCalls = true;
}
else {
    enum bool checkGLCalls = false;
}

enum MAX_GL_VARIABLE_NAME = 256;

// NOTE: Currently used
private auto trust(alias fnc, Args...)(Args args) @trusted => fnc(args);

private {
    bool is_integral(GLType type) pure => (type >= GLType.BYTE && type <= GLType.UINT);
    bool is_floating(GLType type) pure => (GLType.FLOAT || GLType.HALF_FLOAT|| GLType.DOUBLE);
    bool is_base_type(GLType type) pure => (is_integral(type) || is_floating(type));

    bool is_vector(GLType type) pure => type.to!string[0..$-1].endsWith("VEC");
}

GLType to_gltype(GLenum type) => cast(GLType)type;

private template toDType(GLType type) // make this work with vector and matrix types
{
    mixin("alias toDType = %s;".format(type.to!string().toLower()));
}

// Handle base types
private template isGLSLBaseType(T)
{
    // Maybe take into consideration double
    static if (is(T == float)) {
        enum isGLSLBaseType = true;
    }
    else static if (is(T == bool)) {
        enum isGLSLBaseType = true;
    }
    // else static if (isIntegral!T) { // TODO: maybe use integral
     // if integral convert to int
    else static if (is(T == int) || is(T == uint) || is(T == byte) || is(T == ubyte)) {
        enum isGLSLBaseType = true;
    }
    else {
        enum isGLSLBaseType = false;
    }
}

private template isGLVector(T)
{
    static if (is(T == V[N], V, size_t N)) {
        static if (N <= 4)
            enum bool isGLVector = true;
        else
            enum bool isGLVector = false;
    }
    else
        enum bool isGLVector = false;
}

// Doesn't work with struct types
private template TypeInfoGLSL(T)
{
    static if (is(T == Bm[Nm][Mm], Bm, size_t Nm, size_t Mm)) { // is matrix
        // TODO: If size Nm or Mm is <=4 it's invalid
        static if (isGLSLBaseType!Bm) {
            alias TypeInfoGLSL = AliasSeq!("matrix", Bm, Nm, Mm);
        }
        else {
            alias TypeInfoGLSL = AliasSeq!("invalid", void, 0, 0);
        }
    }
    else static if (is(T == Bv[Nv], Bv, size_t Nv)) { // is vector
        static if (isGLSLBaseType!Bv) {
            static if (Nv > 4)
                alias TypeInfoGLSL = AliasSeq!("array", Bv, Nv, 1);
            else
                alias TypeInfoGLSL = AliasSeq!("vector", Bv, Nv, 1);
        }
        else {
            alias TypeInfoGLSL = AliasSeq!("invalid", void, 0, 0);
        }
    }
    else {
        static if (isGLSLBaseType!T) {
            alias TypeInfoGLSL = AliasSeq!("base", T, 1, 1);
        }
        else {
            alias TypeInfoGLSL = AliasSeq!("invalid", void, 0, 0);
        }
    }
}

// TODO: make this work with matrix types
template to_gltype(T)
{
    import std.string   : toUpper;

    alias TInfo = TypeInfoGLSL!T;

    enum string kind = TInfo[0];

    static assert(kind != "invalid");

    alias BT = TInfo[1];
    enum size_t N = TInfo[2];
    enum size_t M = TInfo[3];

    static if (kind == "vector" && N <= 4) {
        static immutable string type_name = BT.stringof.toUpper;
        enum string dims = N.to!string;
        enum string prefix = (type_name[0] == 'F') ? "" : type_name[0..1];
        mixin("enum GLType to_gltype = GLType."~prefix~"VEC"~dims~";");
    }
    else // base type
        mixin("enum GLType to_gltype = GLType.%s;".format(T.stringof.toUpper()));
}

/++
    Returns whether `param` is a parameter that can be passed to `glGetShaderIv`

    Params:
        param = A GLParam or GLenum represting an parameter for a Shader or Program
 +/
@safe nothrow pure
bool is_shader_param(GLParam param)
{
    with(GLParam)
    return (param == SHADER_TYPE || param == DELETE_STATUS ||
           param == COMPILE_STATUS || param == INFO_LOG_LENGTH ||
           param == SHADER_SOURCE_LENGTH
    );
}

/++
    Returns whether `param` is a parameter that can be passed to `glGetProgramiv`

    Params:
        param = A parameter for a Shader or Program
 +/
@safe nothrow pure
bool is_program_param(GLParam param)
{
    with(GLParam)
    return (param == GL_DELETE_STATUS || param == GL_LINK_STATUS ||
            param == GL_VALIDATE_STATUS || param == GL_INFO_LOG_LENGTH ||
            param == GL_ATTACHED_SHADERS
    );
}

/// Clear all opengl error
@trusted @nogc nothrow
private static void opengl_clear_errors()
{
    while(glGetError() != GL_NO_ERROR) {}
}

/// Return opengl error
@trusted nothrow
private GLInternalError opengl_get_error()
{
    while (int my_error = glGetError())
        return cast(GLInternalError)my_error;
    return GLInternalError.NO_ERROR;
}

/++
    Run OpenGL function and log it on version `VADGL_DebugGLCalls`
    Use `gl_wrap` instead for error handling.

    Returns: The result of the opengl function call
 +/
template gl_call(alias fnc)
{
    private import std.traits   : isSomeFunction, ReturnType;
    static assert(isSomeFunction!fnc, "`fnc` must be a function");

    enum string fnc_name = __traits(identifier, fnc);
    alias T = ReturnType!fnc;

    T gl_call(Args...)(Args args)
    {
        version(VADGL_DebugGLCalls)
        {
            // TODO: Maybe use `std.logger` instead. Also make a way to set debug file
            import std.stdio;
            import std.conv         : to;
            import std.exception    : assumeWontThrow;
            // So that I don't have to remove `nothrow` from gl_wrap
            string args_str = "";
            // this could be done better
            static foreach(i, arg; args) {
                args_str ~= assumeWontThrow(arg.to!string);
                static if (i < cast(int)(args.length)-1)
                    args_str ~= ", ";
            }
            assumeWontThrow(writeln(fnc_name, "(", args_str, ")"));
        }
        // Should work even if `T` is void
        return fnc(args);
    }
}

/++
    Run opengl function `fnc_` and check for OpenGL errors

    Returns: `Result!(GLInternalError, T)` where `T` is the return type of
    `fnc_`
 +/
template gl_wrap(alias fnc_)
{
    private import std.traits   : isSomeFunction, ReturnType;

    static if (!isSomeFunction!fnc_)
        alias fnc = fnc_!Args;
    else
        alias fnc = fnc_;

    enum string fnc_name = __traits(identifier, fnc);
    alias T = ReturnType!fnc;
    alias ErrorType = InternalError;
    alias ResultT = Result!(ErrorType, T);

    @trusted nothrow
    ResultT gl_wrap(Args...)(Args args)
    {
        ResultT res = ResultT();

        static if (checkGLCalls)
            opengl_clear_errors();

        static if (!is(T == void)) {
            res = ResultT(gl_call!fnc(args));
        }
        else {
            gl_call!fnc(args);
        }

        static if (checkGLCalls)
            if (GLInternalError err = opengl_get_error())
                return ResultT(ErrorType(cast(GLFuncEnum)fnc_name, err));

        return res;
    }
}

@safe @nogc nothrow pure
private char[10] uint_to_char_buff(uint x)
{
    import std.conv:    toChars;
    char[10] buff = 0;
    auto char_range = x.toChars();
    assert(char_range.length < buff.length);

    int i = 0;
    foreach (char c; char_range) {
        buff[i++] = c;
    }
    return buff;
}

@safe @nogc nothrow pure
private bool member_in_enum(T)(T value) if (is(T == enum))
{
    import std.traits   : EnumMembers;
    switch(value)
    {
        static foreach(member; EnumMembers!T) {
            case member:
                return true;
        }
        default:
            return false;
    }
}

/++
    Return string representation of enum member.
    For example `enum_to_str(MyEnum.A)` return string `"A"`

    NOTES:
    $(LIST
        * this affects compilation times for big enums(>50 members)
        * Doesn't work if enum has duplicates
        * Doesn't work if `value` is not a valid enum member
     )
 +/
@safe @nogc nothrow pure
private string enum_to_str(T)(T value) if (is(T == enum))
{
    import std.conv     : to;
    import std.traits   : EnumMembers;

    final switch(value)
    {
        static foreach(member; EnumMembers!T) {
            case member:
                return member.stringof;
        }
    }
}

// None of `Shader.Type` members are more than 8 characters, but reserve 32 bytes
// anyway. Better safe than sorry
@safe @nogc nothrow pure
private immutable(char)[32] to_char_buff(Shader.Type type)
{
    char[32] buff = 0;
    if (type.member_in_enum()) {
        string str = type.enum_to_str();
        assert(str.length <= 32);  // Should never happen
        buff[0..str.length] = str[];
    }
    else {
        buff[0..10] = uint_to_char_buff(cast(uint)type);
    }
    return buff;
}


private enum isShader(T) = is(T == Shader);
private alias isShaderSeq(Args...) = allSatisfy!(isShader, Args);

/+++
    Abstraction over shader
+++/
struct Shader
{
    import std.bitmanip             : bitfields;
    // TODO: Move this to `types.d`
    enum Type {
        VERTEX           =  GL_VERTEX_SHADER,
        GEOMETRY         =  GL_GEOMETRY_SHADER,
        FRAGMENT         =  GL_FRAGMENT_SHADER,
    }

    private {
        uint id_ = 0;
        string name_ = "";

        Type type_;

        mixin(bitfields!(
            bool, "is_created_", 1,
            bool, "is_source_set_", 1,
            bool, "is_compiled_", 1,
            ubyte, "_padding", 5,
        ));
    }

    @property @safe @nogc nothrow {
        // you can look but not touch ;)
        bool is_created() const => this.is_created_;
        bool is_source_set() const => this.is_source_set_;
        bool is_compiled() const => this.is_compiled_;

        Type type() const => type_;
        uint id() const => this.id_; 
        string name() const => this.name_;
    }

    // This doesn't throw but can't be marked as nothrow bc of `format`
    // Wrapper to glCreateShader
    /+++
        Create OpenGL shader
    +++/
    nothrow
    static GLResult!Shader create_shader(string name, Type type)
    {
        auto res = gl_create_shader(type);
        // error handling
        switch(res.error.error_flag)
        {
            case GLError.Flag.NO_ERROR:
                if (res.value == 0) goto default;
                break;
            case GLError.Flag.INVALID_ENUM:
                return GLResult!Shader(
                    GLError(
                        GLError.Flag.INVALID_ENUM,
                        ": Type "~type.to_char_buff()~" is not a valid shader type"
                    )
                );
            default:
                return GLResult!Shader(GLError(GLError.Flag.UNKNOWN_ERROR));
        }
        Shader shader = Shader(name, type, res.value, true);
        return GLResult!Shader(shader);
    }

    //  TODO: Might want to add file and line as template params
    /+++
        Create OpenGL shader and call `gl_shader_source` to set source in
        shader object
    +++/
    nothrow
    static GLResult!Shader from_src(string name, Type type, string src)
    {
        GLResult!Shader shader_res = Shader.create_shader(name, type);
        if (!shader_res.is_error()) {
            if (auto res = shader_res.value.set_source(src))
                return GLResult!Shader(res.error.append_fnc());
        }
        else
            shader_res = GLResult!Shader(shader_res.error.append_fnc());
        return shader_res;
    }

    // TODO:
    // maybe remove this function or put it a version block
    static GLResult!Shader from_file(string name, Type type, string file_name)
    {
        import std.file;

        auto shader_res = Shader.create_shader(name, type);

        if (shader_res.is_error())
            return GLResult!Shader(shader_res.error.append_fnc());

        Shader shader = shader_res.value;
        if (auto res = shader.set_source(std.file.readText(file_name)))
            return GLResult!Shader(res.error.append_fnc());

        return glresult(shader);
    }

    /++
        Intialize shader with values.

        If you have already successfully called `glCreateShader`,
       `glShaderSource` and `glCompileShader` then you can initialize like so:
        ```d
        auto shader = Shader("my_shader", shader_type, shader_id, all_ok: true);
        ```

        To actually create a shader use `Shader.create_shader`
     +/
    @safe @nogc nothrow
    this
    (string name, Type type, uint id, bool created=false,
     bool source_set=false, bool compiled=false, bool all_ok=false)
    {
        this.name_ = name;
        this.type_ = type;
        this.id_ = id;
        this.is_created_ = created | all_ok;
        this.is_source_set_ = is_source_set_ | all_ok;
        this.is_compiled_ = is_compiled_ | all_ok;
    }

    /+
        Set shader source to `src`
    +/
    // Kind of Wrapper to glShaderSource
    @trusted nothrow
    GLResult!void set_source(in string src)
    {
        // TODO: Add a null check for src

        if (!this.is_created)
            return glresult(GLError(GLError.Flag.INVALID_SHADER));

        if (auto res = gl_shader_source(this.id, src))
            // This is way longer than I expected
            return res.error.append_fnc().glresult();

        this.is_source_set_ = true;
        return GLError(GLError.Flag.NO_ERROR).glresult();
    }

    /* @safe */
    /+++
        compile shader and check whether compilation is successful

        Returns: GLResult!void with error info if an OpenGL error occurred
    +++/
    @trusted nothrow
    GLResult!void compile()
    {
        if (!is_created)
            return GLError(GLError.Flag.INVALID_SHADER).glresult();

        if (!is_source_set)
            return GLError(GLError.Flag.SHADER_SOURCE_NOT_SET).glresult();

        // Assume source is already set
        // should never happen
        if (auto res = gl_compile_shader(this.id))
            return res.error.append_fnc().glresult();

        GLResult!bool compile_res = this.get_param!(GLParam.COMPILE_STATUS);
        if (compile_res)
            return compile_res.error.append_fnc().glresult();

        // NOTE: add get_info_log
        if(!compile_res.value) {
            // Ignore retrun value, no error should occur at this point
            return GLError(
                GLError.Flag.SHADER_COMPILATION_ERROR,
                ": Failed to compile `"~this.name~"` shader\n" ~
                "\tHINT: Run get_info_log() to get extra information"
            ).glresult();
        }

        this.is_compiled_ = true;
        return GLError().glresult();
    }

    /// ditto
    @safe nothrow
    GLResult!void compile_src(in string src)
    {
        if (auto res = this.set_source(src))
            return res.error.append_fnc().glresult();

        if (auto res = this.compile())
            return res.error.append_fnc().glresult();

        return GLError().glresult();
    }

    /// Same as `Shader.compile` except this function can throw
    @safe
    GLResult!void compile_f(in string file_name)
    {
        import std.file;
        // TODO: Handle exception
        string shader_src = std.file.readText(file_name);

        return this.compile_src(shader_src);
    }

    string toString() const
        => "Shader(name: %s, type: %s, id: %d, created: %s, source_set: %s, compiled: %s)"
                .format(
                    this.name, this.type, this.id, this.is_created,
                    this.is_source_set, this.is_compiled);
}

struct Program
{
    import std.bitmanip             : bitfields;
    // Nothing here throws
    nothrow:

    private {
        uint id_ = 0;
        string name_ = "";
        // TODO: might add a way to cache uniforms
        mixin(bitfields!(
            bool, "is_created_", 1,
            bool, "is_attached_", 1,
            bool, "is_linked_", 1,
            bool, "is_validated_", 1,
            ubyte, "_padding", 4,
        ));
    }

    // getters
    @property @safe @nogc {
        bool is_created() const => this.is_created_;
        bool is_attached() const => this.is_attached_;
        bool is_linked() const => this.is_linked_;
        bool is_validated() const => this.is_validated_;

        uint id() const => this.id_; 
        string name() const => this.name_; 
    }

    this(string program_name, int program_id, bool created = false)
    {
        this.name_ = program_name;
        this.id_ = program_id;
        this.is_created_ = created;
    }

    /+++
        Creates program
        Returns: `GLResult!Program` returns valid program or 
        `GLError.Flag.UNKNOWN_ERROR` if an error ocurred
    +++/
    @trusted
    static GLResult!Program create_program(string program_name)
    {
        // Shouldn't be able to fail,
        int prog_id = gl_create_program();

        if (prog_id == 0) { // something went wrong
            return GLResult!Program(GLError(GLError.Flag.UNKNOWN_ERROR));
        }

        Program program = Program(program_name, prog_id, true);
        return program.glresult();
    }

    /+++
        Use null program. same as `gl_program_use(0)`
    +++/
    @safe
    static void use_empty() => cast(void)Program().use();

    /// Attach 1 shader use [Program.attach] instead
    @trusted
    private GLResult!void attach_(Shader s)
    {
        if (!this.is_created) {
            return GLError(GLError.Flag.INVALID_PROGRAM).glresult();
        }

        // program.id should be a valid OpenGL program at this point
        switch(gl_attach_shader(this.id, s.id).error.error_flag) {
            case GLError.Flag.NO_ERROR:
                this.is_attached_ = true;
                return glresult(GLError.NO_ERROR);

            case GLError.Flag.INVALID_OPERATION:
            {
                if (!s.is_created) {
                    return glresult(GLError("glAttachShader", GLError.Flag.INVALID_SHADER));
                }
                else if (!s.is_compiled) {
                    return glresult(GLError("glAttachShader", GLError.Flag.INVALID_SHADER));
                }
                else { // if shader `s` is compiled this is the only possibility
                    return glresult(GLError("glAttachShader", GLError.Flag.SHADER_ALREADY_ATTACHED));
                }
            }
            default:
                return glresult(GLError.UNKNOWN_ERROR);
        }
    }

    // Wrapper to glAttachShader
    // TODO: maybe check GL_ATTACHED_SHADERS to check if number of shaders
    // attached is what we expect
    /++
        attach shaders to Program
        Params:
            shaders = `AliasSeq` of shaders

        Returns: `GLResult!void` with `GLError`:
        $(LIST
            * `GLError.Flag.NO_ERROR`:
                If attached correctly and there are no errors
            * `GLError.Flag.INVALID_PROGRAM`:
                if program hasn't been initialized properly
            * `GLError.Flag.INVALID_SHADER`:
                if shader hasn't been initialized
                if shader hasn't been compiled succesfully
            * `GLError.Flag.SHADER_ALREADY_ATTACHED`:
                if Shader `s` has already been attached to program
        )

        Example:
        ---
        // Initialize program and shaders
        Program program = ...
        Shader vertex_shader = ...
        Shader frag_shader = ...

        // Call [gl_attach_shader] per shader and throw on error
        program.attach(vertex_shader, frag_shader).throw_on_error;
        ---
     +/
    @trusted
    GLResult!void attach(Args...)(Args shaders) if (Args.length > 0 && isShaderSeq!Args)
    {
        foreach (shader; shaders) {
            if (auto res = this.attach_(shader)) return res;
        }
        return GLError.NO_ERROR.glresult();
    }

    // TODO: Maybe check extra cases
    /++
        Link shaders attached to program
        Returns: `GLResult!void` with `GLError`:
        $(LIST
            * `GLError.Flag.NO_ERROR`:
                if no opengl errors occurred
            * `GLError.Flag.INVALID_PROGRAM`:
                if program is valid
            * `GLError.Flag.UNKNOWN_ERROR`:
                if something unexpected went wrong
         )
     +/
    @trusted
    GLResult!void link()
    {
        if (!this.is_attached) { // not a single shader is attached
            return glresult(GLError.INVALID_PROGRAM);
        }

        auto res = gl_link_program(this.id);
        switch(res.error.error_flag)
        {
            case GLError.Flag.NO_ERROR: break;
            case GLError.Flag.INVALID_OPERATION:
                // TODO: Do something else here probably
                return glresult(GLError.UNKNOWN_ERROR); 
            default:
                return glresult(GLError.UNKNOWN_ERROR);
        }

        int linked = 0;
        if (GLError error = this.get_param(GLParam.LINK_STATUS, linked))
            return glresult(error.append_fnc());

        if (!linked) {
            /* string error_msg; */
            /* auto _ = this.get_info_log(error_msg); */
            return glresult(GLError(GLError.Flag.PROGRAM_LINK_ERROR));
        }

        this.is_linked_ = true;
        return glresult(GLError.NO_ERROR);
    }

    // Wrapper to glValidateProgram
    // TODO:
    //  - [Add error hint] take care of potential geometry shader shenanigans later
    /+++
        validate program and call `get_param` to check if validation was
        successful

        Returns: `GLResult!void` with GLError with error information if any
    +++/
    @trusted
    GLResult!void validate()
    {
        if (!this.is_linked) // not a single shader is attached
            return glresult(GLError(GLError.Flag.INVALID_PROGRAM));

        int validated = 0;
        // TODO: Make gl_validate_program function
        // Shouldn't raise any errors
        gl_call!glValidateProgram(this.id);

        if (GLError error = this.get_param(GLParam.VALIDATE_STATUS, validated))
            return glresult(error.append_fnc);

        if (!validated) {
            /* string error_msg; */
            /* auto _ = this.get_info_log(error_msg); */
            return glresult(GLError(
                GLError.Flag.PROGRAM_VALIDATION_ERROR,
                "\tHINT: Run get_info_log() to get extra information"
            ));
        }
        this.is_validated_ = true;
        return glresult(GLError.NO_ERROR);
    }

    /++
        use `this` Program. Use `Program.use_empty()` to use null program

        Returns: `GLResult!void` with GLError with error information if any
     +/
    @trusted
    GLResult!void use() inout
    {
        // NOTE: in theory if this.id_ is 0 it shouldn't be an error in OpenGL
        // but in really makes no sense to use an invalid program.
        // Make a `Program.stop_using()` or `Program.use_empty()`
        return gl_use_program(this.id_);
    }

    // Cool convinience function
    // Might be cool to use a variadic template here with `ref Shader`
    // this solutions is ok thought
    /++
        Compile all `shaders`, attach each to program
        link and validate program.

        $(WARNING
            This function may do to much, it may get deleted or
            renamed. Use the equivalent descripted in [#examples]:
         )

        Returns: `GLResult!void` with error information if any
        Example:
        ---
        program.prepare_and_attach(&vert_shader, &frag_shader).throw_on_error;
        // Program read to use!
        ---
        is equivalent to:
        ---
        Program program;
        Shader vert_sh, frag_shader; // our shaders;
        foreach (shader; [&vert_sh, &frag_shader])
            shader.compile().throw_on_error;

        program.attach(vert_sh, frag_shader).throw_on_error;
        program.link().throw_on_error;
        program.validate().throw_on_error;
        // Program ready to use!
        ---
     +/
    @safe
    GLResult!void prepare_and_attach(Shader*[] shaders ...)
    {
        foreach (Shader *shader; shaders) {
            if (!shader.is_compiled) {
                if (auto res = shader.compile()) return res.error.append_fnc().glresult();
            }

            if (auto res = this.attach(*shader)) return res.error.append_fnc().glresult();
        }

        if (auto res = this.link()) return res.error.append_fnc().glresult();
        if (auto res = this.validate()) return res.error.append_fnc().glresult();

        // Everythin ok
        return glresult(GLError.NO_ERROR);
    }

    // Wrapper to glUniformLocation
    // If loc is negative then uniform `u_name` is not an active uniform
    /+++
        Wrapper to `gl_get_uniform_location`

        Returns: `GLResult!int` with location to uniform and error information
        if any
    +++/
    @trusted
    GLResult!int get_uniform_loc(string u_name) inout /* out(result; result > 0) */
    {
        import std.string   : toStringz;
        GLResult!int loc_result = gl_get_uniform_location(this.id, u_name);
        /* if (loc < 0) { */
        /*     // TODO: use `std.logger` and put this in a version block */
        /*     stderr.writeln("[GL_WARNING]: "~u_name~" is not an active uniform name"); */
        /* } */
        return loc_result;
    }
}

// TODO: Return `GLResult!void` or `Result!(InternalError, void)`
nothrow
GLError get_param(ref const(Shader) self, GLParam param, ref int val)
{
    import std.stdio;
    import std.exception;
    if (!self.is_created)
        return GLError.INVALID_SHADER;

    /* assumeWontThrow(writeln(param, ": val: ", val)); */
    auto res = gl_wrap!glGetShaderiv(self.id, param, &val);
    /* assumeWontThrow(writeln(param, ": val after: ", val)); */
    /* assumeWontThrow(writeln(res)); */

    return res.error.to_glerror();
}

// NOTE: I feel like it might make sense to use GLResult here... 
// NOTE: can't make this auto ref for some reason
// Get param for Program
nothrow
GLError get_param
(ref const(Program) self, GLParam param, out int val)
{
    if (!self.is_created)
        return GLError(GLError.Flag.INVALID_PROGRAM);

    auto res = gl_wrap!glGetProgramiv(self.id, param, &val);

    GLError err = GLError.NO_ERROR;

    with(GLInternalError)
    switch(res.error.error_flag)
    {
        case NO_ERROR: break; // do nothing
        case INVALID_ENUM: goto case INVALID_OPERATION;
        case INVALID_OPERATION:
            err = GLError.INVALID_PARAMETER;
            break;
        default:
            err = GLError.UNKNOWN_ERROR;
            break;
    }
    return err;
}

// TODO: This can be simiplified with a compile time associative array
private alias ParamReturnTypes = AliasSeq!(
    Shader.Type, // SHADER_TYPE
    bool, // DELETE_STATUS
    bool, // COMPILE_STATUS
    int, // INFO_LOG_LENGTH
    int, // SHADER_SOURCE_LENGTH
    bool, // LINK_STATUS
    bool, // VALIDATE_STATUS
);


private template getParamReturnType(GLParam p)
{
    import std.traits   : EnumMembers;

    private template getParamReturnType_(int i, GLParam p)
    {
        static if (i == ParamReturnTypes.length) {
            alias getParamReturnType_ = void;
        }
        else static if (EnumMembers!GLParam[i] == p) // found
        {
            alias getParamReturnType_ = ParamReturnTypes[i];
        }
        else
            alias getParamReturnType_ = getParamReturnType_!(i+1, p);
    }

    alias getParamReturnType = getParamReturnType_!(0, p);
}

/* private template get_param_(S, T) */
/* { */
/*     nothrow */
/*     GLResult!T get_param_(Param p)(ref inout(S) self) */
/*     { */
/*         int _val; */
/*         GLError err = get_param(self, p, _val).append_fnc(); */
/*         return GLResult!T(err, cast(T)_val); */
/*     } */
/* } */

private enum isValidTypeForParam(T, GLParam p) =
        (is(T == Shader) && p.is_shader_param()) ||
        (is(T == Program) && p.is_program_param());

private template get_param_(GLParam p)
{
    alias T = getParamReturnType!p;

    static assert(!is(T == void));

    nothrow
    GLResult!T get_param_(S)(ref inout(S) self) if (isValidTypeForParam!(S, p))
    {
        int _val = -1;
        GLError err = get_param(self, p, _val).append_fnc();
        return GLResult!T(err, cast(T)_val);
    }
}

alias get_param(GLParam p) = get_param_!p;

// TODO: This should return an error in a couple of escenarios
// NOTE: Vertex Array Objects aren't
// available before opengl 3
struct VArrayObject
{
    // this is nogc except if version is VADGL_Debug
    nothrow:

    private uint id_ = 0;

    @property @safe uint id() const => this.id_;

    @safe pure
    this(uint vao_id) { this.id_ = vao_id; }

    // If this is not supposed to be able to fail then why do I return a GLResult
    @trusted
    static GLResult!VArrayObject create()
    {
        uint vao_id;
        gl_call!glGenVertexArrays(1, &vao_id); // Shouldn't be able to fail
        return GLResult!VArrayObject(VArrayObject(vao_id));
    }

    // cannot fail
    void disable() @safe => cast(void)VArrayObject().bind();

    // can fail if id_ is not valid
    GLResult!void bind() @trusted => gl_wrap!glBindVertexArray(id_).to_glresult();
}

struct VBufferObject
{
    // this is nogc except if version is VADGL_Debug
    nothrow:

    private {
        uint id_ = 0;
        GLenum target = GL_ARRAY_BUFFER;
    }

    @trusted
    static GLResult!VBufferObject create(GLenum target = GL_ARRAY_BUFFER)
    {
        uint vbo_id;
        gl_call!glGenBuffers(1, &vbo_id); // Shouldn't be able to fail
        return glresult(VBufferObject(vbo_id, target));
    }

    // This could only fail if target is invalid. But that's ok
    static void disable(GLenum target) @safe => cast(void)VBufferObject().bind(target);

    // Allocates and copys `data` to a gpu buffer
    @trusted
    static GLResult!void set_data(GLenum target, size_t size, const(void*) data, GLenum usage)
    {
        return gl_buffer_data(target, size, data, usage);
    }

    @property uint id() @safe const => this.id_;

    this(uint vbo_id, GLenum target = GL_ARRAY_BUFFER) @safe
    {
        this.id_ = vbo_id;
        this.target = target;
    }

    // can fail if id_ is not valid or if target is not valid
    @trusted
    GLResult!void bind(GLenum target) => gl_wrap!glBindBuffer(target, id_).to_glresult();

    // can fail if id_ is not valid
    @safe
    GLResult!void bind() => bind(this.target);

    void disable() @safe => cast(void)disable(this.target);

    /*
       The OpenGL target often has no effect really so it doesn't matter which you use
    */
    GLResult!void set_data(size_t size, const(void*) data = null, GLenum usage = GL_STATIC_DRAW)
    {
        // TODO: Hmmmmmmm, not sure about this one
        /* if (auto res = this.bind(GL_ARRAY_BUFFER)) */
        /*     return res; */

        /*
            NOTE: I'm using `GL_ARRAY_BUFFER` instead of the buffer type because
            for copying data, because it doesn't make sense to use something else
            anyway. Though I guess this would require me to also bind which may
            cause a problem. But binding just makes the most sense here
        */
        auto res = VBufferObject.set_data(GL_ARRAY_BUFFER, size, data, usage);

        /* VBufferObject.disable(GL_ARRAY_BUFFER); */

        return res;
    }
    // Copy `data` into buffer + offset
    @trusted
    GLResult!void set_sub_data(size_t offset, size_t size, const(void*) data)
    {
        return gl_buffer_sub_data(GL_ARRAY_BUFFER, offset, size, data);
    }


    // Should be safe
    @trusted
    GLResult!void set_data(const(void[]) data, GLenum usage)
        => set_data(data.length, data.ptr, usage);

    @trusted
    GLResult!void set_data(T)(const(T[]) data, GLenum usage) if (!is(T == void) && is(T : void[]))
        => set_data(cast(void[])data, usage);
}

struct GLAttributeInfo
{
    nothrow:

    int loc;
    GLType type;
    int count;
    size_t offset_;
    bool normalized = false;

    static GLResult!GLAttributeInfo from_name
    (int program_id, string name, GLType type, int count, size_t offset_, bool normalized=false)
    {
        GLAttributeInfo attr = GLAttributeInfo(-1, type, count, offset_, normalized);
        if (auto res = attr.set_location(name, program_id))
            return GLResult!GLAttributeInfo(res.error);

        return attr.glresult();
    }

    // TODO: Implement this bullshit
    GLResult!void set_location(string name, int program_id)
    {
        if (auto res = gl_get_attribute_location(name, program_id))
            return res.error.append_fnc().glresult();
        return GLError.no_error().glresult();
    }

    GLResult!void enable()
        => gl_enable_vertex_attributes(this.loc);

    // TODO: A lower version than glSupport could be loaded
    // So add a way to set the actual OpenGL version at runtime
    // check that, and return an error if the version is lower than expected
    static if (glSupport >= GLSupport.gl33)
        GLResult!void set_divisor(uint divisor)
            => gl_vertex_attrib_divisor(this.loc, divisor);

    // Remember about glVertexAttribFormat
    GLResult!void set(size_t stride)
        => gl_vertex_attribute_conf(loc, count, type, stride, offset_, normalized);

    GLResult!void setI(size_t stride)
        => gl_vertex_attributeI_conf(loc, count, type, stride, offset_);
}

/++
    Abstraction over uniforms
 +/
struct GLUniform
{
    int loc; // TODO: perhaps remove `loc`

    static GLResult!GLUniform from_name(string name, uint program_id)
    {
        auto res = gl_get_uniform_location(program_id, name);
        if (res)
            return GLResult!GLUniform(res.error); 
        return GLResult!GLUniform(GLUniform(res.value));
    }

    GLResult!void set_location(string name, int program_id)
    {
        auto res = gl_get_uniform_location(program_id, name);
        if (res)
            return res.error.glresult();
        return GLResult!void();
    }

    GLResult!void set(T)(T value)
    if (T.stringof.among("float", "int", "uint"))
    {
        T[1] val = value; // not great but just easier
        return gl_set_uniform(this.loc, 1, val);
    }

    GLResult!void set_mat4(int n, const(float)[] mat, bool normalized=false)
        => set_uniform_mat4(this.loc, n, mat, normalized);

    // Will copy as much as possible of mat
    GLResult!void set_mat4(const(float)[] mat, bool normalized=false)
        => set_uniform_mat4(this.loc, mat, normalized);

    GLResult!void set_v(int N, T)(T[N] vec, int n = 1)
    if (T.stringof.among("float", "int", "uint") && N <= 4)
        => gl_set_uniform(this.loc, n, vec);

    GLResult!void set_v(int N, T)(T[] vec, int n = 1)
    if (T.stringof.among("float", "int", "uint") && N <= 4)
        => gl_set_uniform!N(this.loc, n, vec);
}

GLResult!void gl_uniform_matrix4fv(int loc, int n, bool normlized, const(float*) mat_ptr)
    => gl_wrap!glUniformMatrix4fv(loc, n, normlized, mat_ptr).to_glresult();

GLResult!void set_uniform_mat4(int loc, int n, const(float[]) mat, bool normalized=false)
in(mat.length >= 16 * n)
    => gl_uniform_matrix4fv(loc, n, normalized, mat.ptr);

GLResult!void set_uniform_mat4(int loc, const(float[]) mat, bool normalized=false)
{
    const int n = cast(int)(mat.length / 16);
    return set_uniform_mat4(loc, n, mat, normalized);
}

private template shortBaseTypeNames(T)
{
    private enum string type_name = T.stringof;
    static assert(type_name.among("float", "int", "uint"));

    static if (type_name == "float" || type_name == "int")
        enum string shortBaseTypeNames = [type_name[0]];
    else
        enum string shortBaseTypeNames = "ui";
}

// Doesn't compile on ldc 1.35 
/* private static immutable string[string] shortBaseTypeNames = [ */
/*     "float": "f", */
/*     "int": "i", */
/*     "uint": "ui" */
/* ]; */

GLResult!void  gl_set_uniform(int N, T)(int loc, int n, T[N] v)
if (T.stringof.among("float", "int", "uint") && N <= 4)
{
    enum string shortT = shortBaseTypeNames!(T);

    return gl_wrap!(mixin("glUniform"~N.to!string~shortT~"v"))(loc, n, v.ptr)
            .to_glresult();
}

GLResult!void gl_set_uniform(int N, T)(int loc, int n, T[] v)
if (T.stringof.among("float", "int", "uint") && N <= 4)
in(v.length >= n * N)
{
    enum string shortT = shortBaseTypeNames!(T);

    return gl_wrap!(mixin("glUniform"~N.to!string~shortT~"v"))(loc, n, v.ptr)
            .to_glresult();
}

GLResult!void  gl_set_uniform(int N, T)(int loc, T[N][] v)
if (T.stringof.among("float", "int", "uint") && N <= 4)
{
    enum string shortT = shortBaseTypeNames!(T);

    return gl_wrap!(mixin("glUniform"~N.to!string~shortT~"v"))(loc, v.length, v.ptr)
            .to_glresult();
}

/* void gl_set_uniform(T, size_t N)(in T[N] vec) */
/* { */
/*     T[N] vec_cp = vec[]; // Copy V since it's immutable */
/*     mixin("glUniform"~N.to~string~TC~"v(loc, 1, vec_cp.ptr);"); */
/* } */

// gl_set_uniform!mat4(0, mpv_mat, true)

// NOTE: This is only an idea
private enum __vadgl_idea = q{
    // TODO: perhaps also assign here the vertex buffer
    // for each of these attributes
    struct Vertex
    {
        @gl_divisor(0) @gl_loc(0) int[2] model_pos;

        @gl_divisor(1):

        @gl_loc(1) int[3] pos;
        @gl_loc(2) @gl_integral ubyte[4] color;
        @gl_loc(3) uint packed_size;
    }

    // TOOD: I could make a mixin template to inject
    // The relevant methods and maybe attributes
    template VertexFormat(VertexType)
    {
        enum size_t AttribCount = getAttribCount!VertexType;

        GLAttribInfo[AttribCount] attributes;
        AttributeType.tupleof[] values;

        GLResult!void set()
        {
            static foreach(...) {
                static if (isIntegral!GL_Type) {
                    attribute.setI();
                }
                else
                    attribute.set();
            }
        }
    }
};

template glattribute(T)
{
    alias TInfo = TypeInfoGLSL!T;

    enum string kind = TInfo[0];

    // TODO: Could also use matrices
    static assert(kind != "invalid");

    alias BT = TInfo[1];
    enum size_t N = TInfo[2];
    enum size_t M = TInfo[3];

    static immutable GLType type = to_gltype!BT;

    @safe @nogc nothrow pure
    GLAttributeInfo glattribute(uint loc, size_t offset_, bool normalized=false)
        => GLAttributeInfo(loc, type, N * M, offset_, normalized);
}

nothrow
GLResult!void gl_buffer_data(GLenum target, GLsizeiptr size, const(void*) data, GLenum usage)
    => gl_wrap!glBufferData(target, size, data, usage).to_glresult();

nothrow
GLResult!void gl_buffer_sub_data(GLenum target, GLintptr offset, GLsizeiptr size, const(void*) data)
    => gl_wrap!glBufferSubData(target, offset, size, data).to_glresult();

/++
    Wrapper to `glCreateShader`
 +/
nothrow
GLResult!uint gl_create_shader(Shader.Type type)
        => gl_wrap!glCreateShader(type).to_glresult();

/++
    Wrapper to `glCreateShader`
 +/
nothrow
GLResult!void gl_shader_source(uint id, int count, const(char**) strings, const(int*) lengths)
    => gl_wrap!glShaderSource(id, count, strings, lengths).to_glresult();

/++
    Wrapper to `glCreateShader`
 +/
nothrow
GLResult!void gl_shader_source(uint id, const(char*) src_c_str, int length)
    => gl_shader_source(id, 1, &src_c_str, &length);

/++
    Wrapper to `glCreateShader`
 +/
nothrow GLResult!void gl_shader_source(uint id, const(char[]) src)
    => gl_shader_source(id, src.ptr, cast(int)src.length);

/++
    Wrapper to `glCreateShader`
 +/
nothrow
GLResult!void gl_compile_shader(uint id)
    => gl_wrap!glCompileShader(id).to_glresult();

/++
    Wrapper to `glCreateShader`
 +/
nothrow
uint gl_create_program()
    => gl_call!glCreateProgram(); // doesn't generate GLError

/++
    Wrapper to `glCreateShader`
 +/
nothrow
GLResult!void gl_use_program(uint program_id)
    => gl_wrap!glUseProgram(program_id).to_glresult();

/++
    Wrapper to `glCreateShader`
 +/
nothrow
GLResult!void gl_attach_shader(uint program_id, uint shader_id)
    => gl_wrap!glAttachShader(program_id, shader_id).to_glresult();

/++
    Wrapper to `glCreateShader`
 +/
nothrow
GLResult!void gl_link_program(uint program_id)
    => gl_wrap!glLinkProgram(program_id).to_glresult();

/*
    NOTE:
    this maps directly to `glVertexAttribPointer` but the name is different.
    I'm not sure if it's better if I change the name closer to it's OpenGL counterpart
*/
// TODO: change error type
nothrow
GLResult!void gl_vertex_attribute_conf
(uint loc, int count, GLType type, size_t stride, size_t offset_, bool normalized = false)
    => gl_wrap!glVertexAttribPointer(
            loc, count, type,
            normalized, cast(GLsizei)stride, cast(void*)offset_).to_glresult();

nothrow
GLResult!void gl_vertex_attributeI_conf
(uint loc, int count, GLType type, long stride, size_t offset_)
    => gl_wrap!glVertexAttribIPointer(
            loc, count, type,
            cast(GLsizei)stride, cast(void*)offset_).to_glresult();

// Maps to `glEnableVertexAttribArray` or `glEnableVertexArrayAttrib`
/*
NOTE (Out of date):
    This function calls `glBindVertexArray` when running on versions < gl45
    Can generate errors:
        - `GL_INVALID_OPERATION`:
            + ~~If no vertex array object is bound.~~
            + if vaobj is not the name of an existing vertex array object.
        - `GL_INVALID_VALUE`:
            + if index is greater than or equal to GL_MAX_VERTEX_ATTRIBS.

NOTE:
    Provide a gl_attribute alternative for this
*/
@trusted nothrow
GLResult!void gl_enable_vertex_attributes(uint[] locations...)
{
    /* Safe by OpenGL sepecs */
    foreach (loc; locations)
        if (auto res = gl_wrap!glEnableVertexAttribArray(loc).to_glresult())
            return res;
    return GLResult!void(GLError.NO_ERROR);
}

nothrow
GLResult!void gl_enable_vertex_array_attributes(uint vao_id, uint[] locations...)
{
    // `glEnableVertexArrayAttrib` only available on version 4.5
    static if (glSupport >= GLSupport.gl45) {
        foreach (loc; locations)
            if (auto res = gl_wrap!glEnableVertexArrayAttrib(vao_id, loc).to_glresult())
                return res;

        return GLResult!void(GLError.NO_ERROR);
    }
    else {
        if (auto res = VArrayObject(vao_id).bind())
            return res; // TODO: Maybe return `GL_INVALID_OPERATION`

        return gl_enable_vertex_attributes(locations);
    }
}

nothrow
GLResult!void gl_enable_vertex_attributes(VArrayObject vao, uint[] locations...)
    => gl_enable_vertex_array_attributes(vao.id, locations);

nothrow
GLResult!void gl_vertex_attrib_divisor(uint loc, uint divisor)
    => gl_wrap!glVertexAttribDivisor(loc, divisor).to_glresult();

nothrow
static GLResult!int gl_get_attribute_location(string name, int program_id)
    => gl_wrap!glGetAttribLocation(program_id, name.toStringz).to_glresult();

/++
    Wrapper to `glGetUniformLocation`
 +/
nothrow
static GLResult!int gl_get_uniform_location(uint program_id, const(char*) name)
    => gl_wrap!glGetUniformLocation(program_id, name).to_glresult();

/*
    NOTE: The only way I could make this @nogc is by adding an arbitrary maximum
    length, allocating a buffer of that size and reserving the last space for
    '\0'.
 */
/++
    Wrapper to `glGetUniformLocation`

    $(WARNING
        This function calls `toStringz` which mayh allocate. Use the
        `const(char*)` overload of this function to avoid allocations
    )
 +/
nothrow
static GLResult!int gl_get_uniform_location(uint program_id, string name)
    => gl_get_uniform_location(program_id, name.toStringz);

// TODO: Replace GLEnum with my own `GLPrimitive` type
/++
    Wrapper to `glDrawArrays`
 +/
nothrow
static GLResult!void gl_draw_arrays(GLenum mode, uint first, int count)
    => gl_wrap!glDrawArrays(mode, first, count).to_glresult();

/++
    Wrapper to `glDrawElements`
 +/
nothrow
static GLResult!void gl_draw_elements(GLenum mode, int count, GLType type, size_t indices)
    => gl_wrap!glDrawElements(mode, count, type, cast(void*)indices).to_glresult();

/++
    Wrapper to `glDrawRangeElements`
 +/
nothrow
GLResult!void gl_draw_range_elements
(GLenum mode, uint start, uint end, int count, GLType type, size_t offset_)
    => gl_wrap!glDrawRangeElements(mode, start, end, count, type, cast(void*)offset_).to_glresult();

/++
    Wrapper to `glDrawArraysInstanced`

    $(NOTE Requires OpenGL version >= 3.1)
 +/
nothrow
GLResult!void gl_draw_arrays_instanced(GLenum mode, int first, int count, int prim_count)
    => gl_wrap!glDrawArraysInstanced(mode, first, count, prim_count).to_glresult();

/++
    Wrapper to `glDrawElementsInstanced`
 +/
nothrow
GLResult!void gl_draw_elements_instanced
(GLenum mode, int count, GLType type, void* indicies_ptr, int prim_count)
    => gl_wrap!glDrawElementsInstanced(mode, count, type, indicies_ptr, prim_count)
            .to_glresult();

/++
    Wrapper to `glClearColor`
 +/
@trusted nothrow
void gl_clear_color(float r=0.0, float g=0.0, float b=0.0, float a=1.0)
    => gl_call!glClearColor(r, g, b, a);


/++
    Wrapper to `glClear`
 +/
@trusted nothrow
GLResult!void gl_clear(GLbitfield mask)
    => gl_wrap!glClear(mask).to_glresult();

// TODO: Handle errors by returning GLResult
// TODO: Could use `Args...` instead of T to bind mutiple stuff at the same time
/// Not sure about this, may get removed later
void gl_with(alias fnc, T)(T globject)
{
    import std.meta : anySatisfy;

    enum bool isT(U) = is(T == U);
    // TODO: Add Texture type
    enum bool isBindable = anySatisfy(isT, VArrayObject, VBufferObject);

    static if (isBindable) globject.bind();
    else static if (is(T == Program)) globject.use();
    fnc();
    static if (isBindable) globject.disable();
    else static if (is(T == Program)) Program.use_empty();
}

/*
    TODO: This is not @nogc because of `fromStringz` and `idup` anway, there's
    no @nogc way to return a string here. Provide an override for whomever
    might want a @nogc version
*/
/++
    Call `glGet{Program|Shader}InfoLog` and return result as a string

    $(WARNING This function allocates)
 +/
@trusted
GLResult!string get_info_log(T)(ref const(T) self)
if (is(T == Shader) || is(T == Program))
{
    import std.format       : sformat;
    import std.string       : fromStringz;
    // Maybe repetitive. But you shouldn't be doing 10_000 glGetInfoLog calls
    // a second, so the extra check does no harm
    if (!self.is_created) {
        return GLResult!string(GLError(GLError.Flag.INVALID_SHADER));
    }

    enum int LOG_MAX_SIZE = 2048;

    char[LOG_MAX_SIZE] error_log = void; // don't bother initializing
    int log_size;

    enum string sh_type = is(T == Shader) ? "Shader" : "Program";

    // Theorethically since self.id should be a valid program/shader
    // at this point and maxLength is > 0 then no OpenGL errors can occur
    auto res = mixin(q{
        gl_wrap!glGet%sInfoLog(
            self.id, LOG_MAX_SIZE, &log_size, error_log.ptr
        ).to_glresult()
    }.format(sh_type));

    if (res)
        return GLResult!string(res.error);

    if (log_size) {
        // TODO: all this could be done in a single buffer.
        char[2048] log_buff = '\0';

        size_t head_len = log_buff[].sformat("Error on \"%s\" shader\n", self.name).length;
        string tail = "\n[LOG SIZE LIMIT REACHED]\0";
        // size to copy from `erro_log`
        size_t log_cp_size =
            (log_size + head_len > 2047)
                ? 2047 - head_len : log_size;

        // could be done better
        log_buff[head_len..head_len+log_cp_size] = error_log[0..log_cp_size];
        log_buff[$-tail.length..$] = tail[];

        return fromStringz(log_buff).idup.glresult();
    }
    return GLResult!string(GLError.NO_ERROR, "");
}
/*
void some_fnc()
{
    gl_with!({
        glBufferData(...)
        glBufferData(...)
    })(vbo)
}
*/
