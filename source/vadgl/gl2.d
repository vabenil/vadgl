module vadgl.gl2;

import bindbc.opengl;

import core.vararg;

// TODO: Put all exceptions inside version blocks
import core.attribute           : mustuse;
import core.exception           : AssertError;
import std.conv                 : to;

import std.traits               : EnumMembers, isIntegral, fullyQualifiedName,
                                  ReturnType, isSomeFunction, isNumeric;

import std.bitmanip             : bitfields;
import std.meta                 : AliasSeq;
// TODO: repalce this function with something simpler
import std.algorithm.searching  : endsWith;
import std.string               : toStringz, fromStringz, toUpper;

import std.format;

import vadgl.types;
import vadgl.error;

import result;

private alias ErrorFlag = GLError.Flag;

enum MAX_GL_VARIABLE_NAME = 256;

// TODO: pls add unittests

auto trust(alias fnc, Args...)(Args args) @trusted => fnc(args);

bool is_integral(GLType type) pure => (type >= GLType.BYTE && type <= GLType.UINT);
bool is_floating(GLType type) pure => (GLType.FLOAT || GLType.DOUBLE);
bool is_base_type(GLType type) pure => (is_integral(type) || is_floating(type));

bool is_vector(GLType type) pure => type.to!string[0..$-1].endsWith("VEC");

GLType to_gl_type(GLenum type) => cast(GLType)type;

template to_gl_type(T) // make this work with vector and matrix types
{
    // TODO: handle matrix types
    static if (is(T == V[N], V, size_t N)) {
        static immutable string type_name = V.stringof.toUpper;
        enum string dims = N.to!string;
        enum string prefix = (type_name[0] == 'F') ? "" : type_name[0..1];
        mixin("enum GLType to_gl_type = GLType."~prefix~"VEC"~dims~";");
    }
    else
        mixin("enum GLType to_gl_type = GLType.%s;".format(T.stringof.toUpper()));
}

template toDType(GLType type) // make this work with vector and matrix types
{
    mixin("alias toDType = %s;".format(type.to!string().toLower()));
}

@safe pure
private string to_err_msg(GLInternalError error)
        => error.to!string ~ " : Error code : " ~ (cast(int)error).to!string;

@safe nothrow pure
bool is_shader_param(GLParam param)
{
    with(GLParam)
    return (param == SHADER_TYPE || param == DELETE_STATUS ||
           param == COMPILE_STATUS || param == INFO_LOG_LENGTH ||
           param == SHADER_SOURCE_LENGTH
    );
}

@safe nothrow pure
bool is_program_param(GLParam param)
{
    with(GLParam)
    return (param == GL_DELETE_STATUS || param == GL_LINK_STATUS ||
            param == GL_VALIDATE_STATUS || param == GL_INFO_LOG_LENGTH ||
            param == GL_ATTACHED_SHADERS
    );
}

@trusted @nogc nothrow
private static void opengl_clear_errors()
{
    while(glGetError() != GL_NO_ERROR) {}
}

@trusted @nogc nothrow
private GLInternalError opengl_get_error()
{
    while (int my_error = glGetError())
        return cast(GLInternalError)my_error;
    return GLInternalError.NO_ERROR;
}

/++
    Run opengl command and return result of `glGetError()`.

    Returns:
        glError if `fnc`'s return type is void or
        Result!(
+/
template gl_wrap(alias fnc_)
{
    static if (!isSomeFunction!fnc_)
        alias fnc = fnc_!Args;
    else
        alias fnc = fnc_;

    static assert(isSomeFunction!fnc, "`fnc_` must be a function");

    enum string func_name = __traits(identifier, fnc);

    alias T = ReturnType!fnc;
    alias ErrorType = InternalError;
    alias ResultT = Result!(ErrorType, T);

    static if (is(T == void))
    ResultT gl_wrap(Args...)(Args args) nothrow
    {
        opengl_clear_errors();
        fnc(args);
        return ResultT(ErrorType(opengl_get_error()));
    }

    static if (!is(T == void))
    ResultT gl_wrap(Args...)(Args args) nothrow
    {
        opengl_clear_errors();

        T ret = fnc(args);

        if (GLInternalError err = opengl_get_error())
            return ResultT(ErrorType(err));

        return ResultT(ret);
    }
}

void gl_assert
(alias func, string file = __FILE__, uint line = __LINE__, Args...)(Args args)
{
    opengl_clear_errors();
    func(args);
    GLInternalError gl_err = opengl_get_error();
    // requires gcc
    if(gl_err != GLInternalError.NO_ERROR) {
        throw new AssertError("[GL_ERROR]: " ~ gl_err.to!string, file, line);
    }
}

// TODO: remove @trusted. Function is not @trusted if expression is not trusted
@trusted
void gl_assert_run(string file = __FILE__, uint line = __LINE__)
(scope void delegate() expression)
{
    opengl_clear_errors();
    expression();
    GLInternalError gl_err = opengl_get_error();
    if(gl_err != GLInternalError.NO_ERROR) {
        throw new AssertError("[GL_ERROR]: " ~ gl_err.to!string, file, line);
    }
}

struct Shader
{
    enum Type {
        VERTEX           =  GL_VERTEX_SHADER,
        GEOMETRY         =  GL_GEOMETRY_SHADER,
        FRAGMENT         =  GL_FRAGMENT_SHADER,
    }

    private {
        uint id_ = 0;
        string name_ = "";

        Type type_;
        string src = null;

        mixin(bitfields!(
            bool, "is_created_", 1,
            bool, "is_source_set_", 1,
            bool, "is_compiled_", 1,
            ubyte, "_padding", 5,
        ));
    }

    @property @safe @nogc nothrow {
        // you can look but not touch ;)
        bool is_created() inout => this.is_created_;
        bool is_source_set() inout => this.is_source_set_;
        bool is_compiled() inout => this.is_compiled_;

        Type type() inout => type_;
        uint id() inout => this.id_; 
        string name() inout => this.name_;
    }

    static
    GLResult!Shader create_shader(string name, Type type)
    {
        auto res = gl_wrap!glCreateShader(cast(uint)type);
        enum string error_header = "[GL_ERROR]: glCreateShader";
        // error handling
        switch(res.error.error_flag)
        {
            case GLInternalError.NO_ERROR:
                if (res.value == 0)
                    goto default;
                break;
            case GLInternalError.INVALID_ENUM:
                return GLResult!Shader(
                        GLError(
                            "glCreateShader",
                            ErrorFlag.INVALID_ENUM,
                            "Type %s is not a valid shader type".format(type)));
            default:
                return GLResult!Shader(GLError("create_shader", ErrorFlag.UNKNOWN_ERROR));
        }
        Shader shader = Shader(name, type, res.value, true);
        return GLResult!Shader(shader);
    }

    // TODO: might want to attach the shader source on the fly in order
    //       to avoid storing the `src`. Who knows how big it could be
    //
    //  Also might want to add file and line as template params
    static GLResult!Shader from_src(string name, Type type, string src)
    {
        GLResult!Shader shader_res = Shader.create_shader(name, type);
        if (!shader_res.is_error())
            if (auto res = shader_res.value.set_source(src))
                return GLResult!Shader(res.error);
        return shader_res;
    }

    // TODO:
    // maybe remove this function or put it a version block
    static GLResult!Shader from_file(string name, Type type, string file_name)
    {
        import std.file;

        auto shader_res = Shader.create_shader(name, type);
        if (shader_res.is_error())
            return shader_res;

        auto shader = shader_res.value;

        if (auto res = shader.set_source(std.file.readText(file_name)))
            return GLResult!Shader(res.error);

        return glresult(shader);
    }

    @safe @nogc nothrow 
    this(string name, Type type, uint id, bool created = false)
    {
        this.name_ = name;
        this.type_ = type;
        this.id_ = id;
        this.is_created_ = created;
    }

    @trusted
    GLResult!void set_source(in string src)
    {
        if (!this.is_created)
            return glresult(GLError("set_source", ErrorFlag.INVALID_SHADER));

        immutable(char*) src_ptr = src.ptr;
        int len = cast(int)src.length;

        if (auto err = gl_wrap!glShaderSource(this.id_, 1, &src_ptr, &len).error) {
            return glresult(GLError("glShaderSource", err.to_glerror_flag()));
        }

        this.is_source_set_ = true;
        return glresult(GLError.NO_ERROR);
    }

    /* @safe */
    @trusted
    GLResult!void compile()
    {
        if (!is_created)
            return glresult(GLError("compile", ErrorFlag.INVALID_SHADER));

        if (!is_source_set)
            return glresult(GLError("compile", ErrorFlag.SHADER_SOURCE_NOT_SET));

        // Assume source is already set
        // should never happen
        if (auto err = gl_wrap!glCompileShader(this.id).error)
            return glresult(GLError("glCompileShader", err.to_glerror_flag()));

        bool compiled = 0;
        if (GLError error = this.get_param!(GLParam.COMPILE_STATUS)(compiled))
            return glresult(error.append("\t[From]: compile\n"));

        // NOTE: add get_info_log
        if(!compiled) {
            // Ignore retrun value, no error should occur at this point
            return glresult(GLError(
                        "compile", ErrorFlag.SHADER_COMPILATION_ERROR,
                        "\tHINT: Run get_info_log() to get extra information"));
        }

        this.is_compiled_ = true;
        return glresult(GLError.NO_ERROR);
    }

    @safe
    GLResult!void compile_src(in string src)
    {
        if (auto res = this.set_source(src))
            return glresult(res.error.append("[From]: compile_src\n"));


        /* error = this.compile(); */
        /* if (error.is_error()) return error; */
        if (auto res = this.compile())
            return glresult(res.error.append("[From]: compile_src\n"));

        return glresult(GLError.NO_ERROR);
    }

    // NOTE: this functions might be redundant, also it throws
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
                    .format(this.name, this.type, this.id, this.is_created,
                            this.is_source_set, this.is_compiled);
}

struct Program
{
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

    @property @safe @nogc nothrow {
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

    static GLResult!Program create_program(string program_name)
    {
        int prog_id = trust!glCreateProgram();

        if (prog_id == 0) { // something went wrong
            return GLResult!Program(GLError.UNKNOWN_ERROR);
        }

        Program program = Program(program_name, prog_id, true);
        return glresult(program);

    }

    // TODO: maybe check GL_ATTACHED_SHADERS to check if number of shaders
    // attached is what we expect
    @trusted
    GLResult!void attach(ref Shader s)
    {
        if (!this.is_created) {
            return glresult(GLError.INVALID_PROGRAM);
        }

        // program.id should be a valid OpenGL program at this point
        switch(gl_wrap!glAttachShader(this.id_, s.id_).error.error_flag) {
            case GLInternalError.NO_ERROR:
                this.is_attached_ = true;
                return glresult(GLError.NO_ERROR);

            case GLInternalError.INVALID_OPERATION:
            {
                if (!s.is_created) {
                    return glresult(GLError("glAttachShader", ErrorFlag.INVALID_SHADER));
                }
                else if (!s.is_compiled) {
                    return glresult(GLError("glAttachShader", ErrorFlag.INVALID_SHADER));
                }
                else { // if shader `s` is compiled this is the only possibility
                    return glresult(GLError("glAttachShader", ErrorFlag.SHADER_ALREADY_ATTACHED));
                }
            }
            default:
                return glresult(GLError.UNKNOWN_ERROR);
        }
    }

    // TODO: finsih fixing glresult here
    @trusted
    GLResult!void link()
    {
        if (!this.is_attached) { // not a single shader is attached
            return glresult(GLError.INVALID_PROGRAM);
        }

        opengl_clear_errors();
        glLinkProgram(this.id_);

        GLInternalError internal_err = opengl_get_error();
        switch(internal_err)
        {
            case GLInternalError.NO_ERROR: break;
            case GLInternalError.INVALID_OPERATION:
                return glresult(GLError.UNKNOWN_ERROR); 
            default:
                return glresult(GLError.UNKNOWN_ERROR);
        }

        int linked = 0;
        if (GLError error = this.get_param(GLParam.LINK_STATUS, linked))
            return glresult(error);

        if (!linked) {
            /* string error_msg; */
            /* auto _ = this.get_info_log(error_msg); */
            return glresult(GLError.PROGRAM_LINK_ERROR);
        }

        this.is_linked_ = true;
        return glresult(GLError.NO_ERROR);
    }

    // TODO:
    //  1 Hmm, maybe I shouldn't throw in here and return bool instead
    //  2) take care of potential geometry shader shenanigans later
    @trusted
    GLResult!void validate()
    {
        if (!this.is_linked) // not a single shader is attached
            return glresult(GLError.INVALID_PROGRAM);

        int validated = 0;
        // Shouldn't raise any errors
        glValidateProgram(this.id);

        if (GLError error = this.get_param(GLParam.VALIDATE_STATUS, validated))
            return glresult(error);

        if (!validated) {
            /* string error_msg; */
            /* auto _ = this.get_info_log(error_msg); */
            return glresult(GLError.PROGRAM_VALIDATION_ERROR);
        }
        this.is_validated_ = true;
        return glresult(GLError.NO_ERROR);
    }

    @trusted
    void use() inout
    {
        // NOTE: in theory if this.id_ is 0 it shouldn't be an error in OpenGL
        // but it really makes no sense to use an invalid program.
        // Make a `Program.stop_using()` or `Program.use_empty()`
        gl_assert!glUseProgram(this.id_);
    }

    // Might be cool to use a variadic template here with `ref Shader`
    // this solutions is ok thought
    @safe
    GLResult!void prepare_and_attach(Shader*[] shaders ...)
    {
        foreach (Shader *shader; shaders) {
            // TODO: do something with error here
            // if a shader fails compilation exit
            if (auto res = shader.compile()) return res;
            if (auto res = this.attach(*shader)) return res;
        }

        if (auto res = this.link()) return res;
        if (auto res = this.validate()) return res;

        // Everythin ok
        return glresult(GLError.NO_ERROR);
    }

    @trusted
    int get_uniform_loc(string u_name) inout
    /* out(result; result > 0) */
    {
        import std.stdio;
        // TODO: add checks and stuff
        int loc = 0;
        gl_assert_run({
            loc = glGetUniformLocation(this.id, u_name.toStringz);
        });
        if (loc < 0) {
            // TODO: use `std.logger` and put this in a version block
            stderr.writeln("[GL_WARNING]: "~u_name~" is not an active uniform name");
        }
        return loc;
    }
}

alias ParamReturnTypes = AliasSeq!(
    Shader.Type, // SHADER_TYPE
    bool, // DELETE_STATUS
    bool, // COMPILE_STATUS
    int, // INFO_LOG_LENGTH
    int, // SHADER_SOURCE_LENGTH
    bool, // LINK_STATUS
    bool, // VALIDATE_STATUS
);

GLError get_param
(ref inout(Shader) self, GLParam param, out int val,
 string file = __FILE__, size_t line = __LINE__)
{
    if (!self.is_created)
        return GLError.INVALID_SHADER;

    opengl_clear_errors();
    glGetShaderiv(self.id, cast(int)param, &val);

    GLError err = GLError.NO_ERROR;
    GLInternalError internal_err = opengl_get_error();

    with(GLInternalError)
    switch(internal_err)
    {
        case NO_ERROR: break; // do nothing
        case INVALID_ENUM:
        {
            err = GLError.INVALID_PARAMETER;
        } break;
        default:
        {
            err = GLError.UNKNOWN_ERROR;
        }
    }
    return err;
}

// NOTE: I feel like it might make sense to use GLResult here... 
// NOTE: can't make this auto ref for some reason
// Get param for Program
GLError get_param
(ref inout(Program) self, GLParam param, out int val)
{
    if (!self.is_created)
        return GLError.INVALID_PROGRAM;

    opengl_clear_errors();
    glGetProgramiv(self.id, cast(int)param, &val);

    GLError err = GLError.NO_ERROR;
    GLInternalError internal_err = opengl_get_error();

    with(GLInternalError)
    switch(internal_err)
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

// NOTE: this would be much better as a mixin template,
// but for some reason mixin template overriding doesn't work
template generateGetParam(S, GLParam Sp, T) // (S)pecialized types
{
    enum string t_name = fullyQualifiedName!T;
    enum string generateGetParam = `
    GLError get_param(GLParam p : GLParam.`~Sp.to!string~`)
    (ref inout(`~S.stringof~`) self, out `~t_name~` val)
    {
        int _val;
        // Could have used scope exit
        GLError err = get_param(self, p, _val);
        val = cast(`~t_name~`)_val;
        return err;
    }
    `;
}

// NOTE: It might be wasteful to create one for every type.
// I can just create a normal template
// Generate specialized get_param functions for every OpenGL parameter
static foreach (i, param; EnumMembers!GLParam)
static if (i < ParamReturnTypes.length) {
    static if (param.is_shader_param())
        mixin(generateGetParam!(Shader, param, ParamReturnTypes[i]));

    static if (param.is_program_param())
        mixin(generateGetParam!(Program, param, ParamReturnTypes[i]));
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
    else static if (is(T == int) || is(T == uint)) { // if integral convert to int
        enum isGLSLBaseType = true;
    }
    else {
        enum isGLSLBaseType = false;
    }
}

// TODO: Return GLResult here
@trusted
GLError get_info_log(T)(ref inout(T) self, out string error_msg)
if (is(T == Shader) || is(T == Program))
{
    // Maybe repetitive. But you shouldn't be doing 10_000 glGetInfoLog calls
    // a second, so the extra check does no harm
    if (!self.is_created) {
        return GLError.INVALID_SHADER;
    }

    enum int LOG_MAX_SIZE = 2048;

    char[LOG_MAX_SIZE] error_log = void; // don't bother initializing
    int log_size;

    enum string sh_type = is(T == Shader) ? "Shader" : "Program";

    // Theorethically since self.id should be a valid program/shader
    // at this point and maxLength is > 0 then no OpenGL errors can occur
    mixin(
        "glGet"~sh_type~"InfoLog(
            self.id, LOG_MAX_SIZE, &log_size, error_log.ptr
        );"
    );

    if (log_size) {
        // TODO: all this could be done in a single buffer.
        char[2048] log_buff = void; log_buff[] = '\0';

        size_t head_len = log_buff[].sformat("Error on \"%s\" shader\n", self.name).length;
        string tail = "\n[LOG SIZE LIMIT REACHED]\0";
        // size to copy from `erro_log`
        size_t log_cp_size =
            (log_size + head_len > 2047)
                ? 2047 - head_len : log_size;

        // could be done better
        log_buff[head_len..head_len+log_cp_size] = error_log[0..log_cp_size];
        log_buff[$-tail.length..$] = tail[];

        error_msg = fromStringz(log_buff).idup;
    }
    return GLError.NO_ERROR;
}

// TODO: handle structs
// Returns either "invalid" if type is not a valid glsl type
// if T is a 2d array AliasSeq!("matrix", Base Type, N, M) is returned
// if T a 1d array AliasSeq!("vector", Base Type, N) is returned
// if T a valid glsl base type AliasSeq!("base", T) is returned
private template getGLSLTypeInfo(T)
{
    static if (is(T == Bm[Nm][Mm], Bm, size_t Nm, size_t Mm)) { // is matrix
        static if (isGLSLBaseType!Bm) {
            alias getGLSLTypeInfo = AliasSeq!("matrix", Bm, Nm, Mm);
        }
        else {
            alias getGLSLTypeInfo = AliasSeq!("invalid");
        }
    }
    else static if (is(T == Bv[Nv], Bv, size_t Nv)) { // is vector
        static if (isGLSLBaseType!Bv) {
            alias getGLSLTypeInfo = AliasSeq!("vector", Bv, Nv, 1);
        }
        else {
            alias getGLSLTypeInfo = AliasSeq!("invalid", void, 0, 0);
        }
    }
    else {
        static if (isGLSLBaseType!T) {
            alias getGLSLTypeInfo = AliasSeq!("base", T, 1, 1);
        }
        else {
            alias getGLSLTypeInfo = AliasSeq!("invalid", void, 0, 0);
        }
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

private template validateGLTypes(BT, Args...)
{
    static if (Args.length == 0) {
        enum bool validateGLTypes = true;
    }
    else static if (is(Args[0] == BT)) {
        enum bool validateGLTypes = validateGLTypes!(BT, Args[1..$]);
    }
    else {
        enum bool validateGLTypes = false;
    }
}

private template validateGLArgs(BT, size_t  N, Args...)
{
    static if (Args.length == N) { // correct size
        enum bool validateGLArgs = validateGLTypes!(BT, Args);
    }
    else {
        enum bool validateGLArgs = false;
    }
}

private template typeToShort(T)
{
    enum string Tname = T.stringof;
    enum string typeToShort = (Tname[0] == 'u') ? "ui" : Tname[0..1].idup;
}

/* TODO:
    - Add error handling
    - Stop using template, store type as GLType and use ditto template to set it
*/
struct GLUniform(T)
{
    // Type info
    private alias TInfo = getGLSLTypeInfo!T;
    // Storage type
    private enum string ST = TInfo[0];
    static assert(
        ST != "invalid",
        "T expected to be a matrix, vector or base type got "~T.stringof~" instead"
    );
    // Base type
    alias BT = TInfo[1];

    enum size_t N = TInfo[2];
    enum size_t M = TInfo[3];

    // true as long as ST is not a matrix or struct
    private enum bool isSimple = (ST == "vector" || ST == "base");
    // char representation of type
    private enum string TC = typeToShort!BT;

    private {
        bool is_location_set = false;
        bool is_program_set = false;

        string name = "";
        int program_id = 0;
        int loc = 0;
    }

    @property int location() => this.loc;

    @property void program(int program)
    {
        // TODO:
        // validate the program
        this.program_id = program;
        // to false if program isn't valid
        this.is_program_set = true;
    }
    @property int program() => program_id;

    // Uniform location can't be known until the program is actually ran
    // because it cant be set in the shader
    this(string name, int program_id=0, int loc=0)
    {
        this.name = name;
        this.program_id = program_id;
        this.loc = loc;

        this.is_location_set = (loc >= 0);
        this.is_program_set = (program_id >= 0);
    }

    // set the `program_id` and `location` of a partial GLUniform
    GLResult!void initialize(int program_id)
    {
        if (program_id <= 0)
            return GLResult!void(GLError.INVALID_PROGRAM);

        this.program = program_id;
        // TODO: this can result in an error handle it.
        GLResult!int res = get_location();
        if (res.is_error())
            return GLResult!void(res.error);

        this.loc = res.value;

        return GLResult!void(GLError.NO_ERROR);
    }

    static GLResult!int get_location(string name, int program_id)
    {
        return gl_wrap!glGetUniformLocation(program_id, name.toStringz).to_glresult();
    }

    GLResult!int get_location() => get_location(this.name, this.program_id);

    @nogc nothrow
    void set(Types...)(Types args) if (validateGLArgs!(BT, N, Types) && isSimple)
    {
        mixin("glUniform"~N.to!string~TC~"(loc, args);");
    }

    @trusted @nogc nothrow
    void set(Tv, size_t Nv)(in Tv[Nv] v) if (ST == "vector" && is(Tv[Nv] == T))
    {
        T v_cp = v[]; // copy v since it's immutable
        mixin("glUniform"~N.to!string~TC~"v(loc, 1, v_cp.ptr);");
    }

    @trusted
    void set(Tv)(in Tv[] v) if (ST == "vector")
    {
        T v_cp = v[]; // copy v since it's immutable
        mixin("glUniform"~N.to!string~TC~"v(loc, 1, v_cp.ptr);");
    }

    void set(Tm, size_t Nm, size_t Mm)(in Tm[Nm][Mm] mat, bool transpose = false)
    if (ST == "matrix" && is(Tm[Nm][Mm] == T))
    {
        Tm[Nm * Mm] v_cp = (cast(Tm[Nm * Mm])mat)[]; // copy v since it's immutable
        mixin("glUniformMatrix"~N.to!string~TC~"v(loc, 1, transpose, v_cp.ptr);");
    }

    @trusted @nogc nothrow
    BT get()() if (ST == "base")
    {
        BT value;
        mixin("glGetUniform"~TC~"v(program_id, loc, &value);");
        return value;
    }

    @trusted @nogc nothrow
    BT[N] get()() if (ST == "vector")
    {
        BT[N] value;
        mixin("glGetUniform"~TC~"v(program_id, loc, &value[0]);");
        return value;
    }
}

struct GLAttribute
{
    private {
        // could use bitmap
        mixin(bitfields!(
            bool,  "is_program_set",    1,
            bool,  "is_location_set",   1,
            bool,  "is_type_set",       1,
            bool,  "is_name_set",       1,
            bool,  "is_initialized_",   1,
            ubyte,  "",                 3, // padding
        ));

        // this stuff shouldn't change after initialization
        int loc = 0;
        string name = "";
        int program_id = 0;
        GLType gl_type = GLType.FLOAT; // will be set later
    }

    @property {
        GLType type() const => this.gl_type;
        int program() const => this.program_id;
        int location() const => this.loc;
        bool is_initialized() const => this.is_initialized_;
    }

    GLResult!void set_program(int program_id)
    {
        if (is_program_set) return GLResult!void(GLError.NO_ERROR);

        if (program_id <= 0)
            return GLResult!void(GLError.INVALID_PROGRAM);

        this.program_id = program_id;
        this.is_program_set = true;

        return GLResult!void(GLError.NO_ERROR);
    }

    this(int loc) in(loc >= 0)
    {
        this.loc = loc;
        this.is_location_set = true;
    }

    this(string name) in(name != "")
    {
        this.name = name;
        this.is_name_set = true;
    }

    this(string name, int loc) in(name != "" && loc >= 0)
    {
        this.loc = loc;
        this.name = name;
        this.is_location_set = true;
        this.is_name_set = true;
    }

    this(string name, int loc, GLType gl_type, int program_id = 0)
    {
        this(name, loc);

        this.gl_type = gl_type;
        this.is_type_set = true;

        this.program_id = program_id;
        this.is_program_set = (program_id > 0);
    }

    this(string name, GLType gl_type, int loc = 0) { this(name, loc, gl_type); }

    // TODO: Check if location is actually created by opengl
    // for now only sets the location
    // but using glGetActiveAttrib get the type of the attribute
    GLResult!void initialize(int program_id)
    {
        if (auto error = set_program(program_id))
            return error;

        if (auto error = set_location())
            return error;

        // get type
        this.is_initialized_ = true;
        this.is_program_set = true;
        return GLResult!void(GLError.NO_ERROR);
    }

    GLResult!void initialize() => this.initialize(this.program_id);

    GLResult!void set_location()
    {
        if (!this.is_program_set) return GLResult!void(GLError.INVALID_PROGRAM);

        if (!this.is_location_set) {
            GLResult!int result = get_location(this.name, this.program);
            if (result.is_error())
                return GLResult!void(result.error);

            this.loc = result.value;
        }

        return GLResult!void(GLError.NO_ERROR);
    }

    static GLResult!int get_location(string name, int program_id)
    {
        return gl_wrap!glGetAttribLocation(program_id, name.toStringz).to_glresult();
    }

    // this is the mirror to the glGetActiveAttrib
    // create and initialze Attribute
    static GLResult!GLAttribute from_location(int loc, int program_id)
    {
        int length;
        int size;
        GLType type;
        // I don't know if I should check what the maximum length for a variable
        // in OpenGL is. But it shouldn't be more than 256 characters, right?
        char[MAX_GL_VARIABLE_NAME] name_buffer;

        // wanted to do it in scopes but it looks kinda ugly that way
        auto info_res = gl_wrap!glGetActiveAttrib(
            program_id, loc, MAX_GL_VARIABLE_NAME,
            &length, &size, cast(GLenum *)&type, &name_buffer[0]
        );

        if (info_res.is_error())
            return GLResult!GLAttribute(info_res.error.to_glerror());

        string name_ = name_buffer[0..length].idup;
        GLAttribute attr = GLAttribute(name_, loc, type, program_id);

        auto init_res = attr.initialize();
        if (init_res.is_error())
            return GLResult!GLAttribute(init_res.error);

        return GLResult!GLAttribute(attr);
    }

    // TODO: Prob don't call from_location here
    static GLResult!GLAttribute from_name(string name, int program_id)
    {
        auto res = get_location(name, program_id);
        if (res.is_error())
            return GLResult!GLAttribute(res.error);

        return from_location(res.value, program_id);
    }

    static GLResult!void enable(int loc)
        => gl_wrap!glEnableVertexAttribArray(loc).to_glresult();

    static GLResult!void disable(int loc)
        => gl_wrap!glDisableVertexAttribArray(loc).to_glresult();
}

// ditto
template gl_attribute(string name, T, uint location = 0)
{
    alias TInfo = getGLSLTypeInfo!T;
    enum string kind = TInfo[0]; // TODO: rename
    alias BT = TInfo[1];
    static assert(location >= 0);

    static assert(kind != "invalid");
    enum GLAttribute gl_attribute = GLAttribute(name, to_gl_type!BT, location);
}

// ditto
GLAttribute gl_attribute(T)(string name, int prog_id, int loc = 0)
if (getGLSLTypeInfo!T[0] != "invalid")
{
    return GLAttribute(name, prog_id, loc, to_gl_type!T);
}

/*
    NOTE:
    this maps directly to `glVertexAttribPointer` but the name is different.
    I'm not sure if it's better if I change the name closer to it's OpenGL counterpart
*/
// TODO: change error type
GLResult!void gl_vertex_attribute_conf
(uint loc, int count, GLType type, size_t stride, size_t offset_, bool normalized = false)
    => gl_wrap!glVertexAttribPointer(
            loc, count, cast(GLenum)type,
            normalized, cast(GLsizei)stride, cast(void*)offset_).to_glresult();

GLResult!void gl_vertex_attributeI_conf
(uint loc, int count, GLType type, size_t stride, size_t offset_)
    => gl_wrap!glVertexAttribIPointer(
            loc, count, cast(GLenum)type,
            cast(GLsizei)stride, cast(void*)offset_).to_glresult();

// TODO: pair this with an attribute so that I can actually know what function to run
template gl_vertex_attribute_conf(T)
{
    static if (is(T == V[Vn], V, size_t Vn))
    {
        alias BT = V; // base type
        enum size_t N = Vn;
    }
    else {
        alias BT = V;
        enum size_t N = 1;
    }

    static assert(isNumeric!BT, "Base type must be numeric");
    static assert(N <= 4, "OpenGL vectors can't have more than 4 dimensions");

    // TODO: Check if gl_attribute has it's location initialized
    // this should only work if attr has `location` and `type` set
    GLResult!void gl_vertex_attribute_conf
    (GLAttribute attr, size_t stride, size_t offset_, bool normalized=false)
    {
        if (attr.type.is_integral)
            return gl_vertex_attributeI_conf(attr.location, N, to_gl_type!BT, stride, offset_);

        return gl_vertex_attribute_conf(attr.location, N, to_gl_type!BT, stride, offset_, normalized);
    }
}

// NOTE: Vertex Array Objects aren't
// available before opengl 3
struct VArrayObject
{
    private {
        uint id_ = 0;
    }

    @property uint id() @safe const => this.id_;

    this(uint vao_id) { this.id_ = vao_id; }

    static GLResult!VArrayObject create()
    {
        uint vao_id;
        glGenVertexArrays(1, &vao_id); // Shouldn't be able to fail
        return GLResult!VArrayObject(VArrayObject(vao_id));
    }

    // can fail if id_ is not valid
    GLResult!void bind() => gl_wrap!glBindVertexArray(id_).to_glresult();

    // cannot fail
    static void disable() => cast(void)VArrayObject().bind();
}

struct VBufferObject
{
    private {
        uint id_ = 0;
        GLenum target = GL_ARRAY_BUFFER;
    }

    // remember that buffer can also take a kind of buffer
    static GLResult!VBufferObject create(GLenum target = GL_ARRAY_BUFFER)
    {
        uint vbo_id;
        glGenBuffers(1, &vbo_id); // Shouldn't be able to fail
        return glresult(VBufferObject(vbo_id, target));
    }

    // This could only fail if target is invalid. But that's ok
    static void disable(GLenum target) => cast(void)VBufferObject().bind(target);

    static GLResult!void set_data(GLenum target, size_t size, void* data, GLenum usage)
    {
        return gl_wrap!glBufferData(target, size, data, usage).to_glresult();
    }

    @property uint id() @safe const => this.id_;

    this(uint vbo_id, GLenum target = GL_ARRAY_BUFFER)
    {
        this.id_ = vbo_id;
        this.target = target;
    }

    // can fail if id_ is not valid or if target is not valid
    GLResult!void bind(GLenum target) => gl_wrap!glBindBuffer(target, id_).to_glresult();

    // can fail if id_ is not valid
    GLResult!void bind() => bind(this.target);

    /*
       The target is not that important we are only interested in setting the data
   */
    GLResult!void set_data(size_t size, void* data = null, GLenum usage = GL_STATIC_DRAW)
    {
        if (auto res = this.bind(GL_ARRAY_BUFFER))
            return res;

        auto res = VBufferObject.set_data(GL_ARRAY_BUFFER, size, data, usage);

        VBufferObject.disable(GL_ARRAY_BUFFER);

        return res;
    }

    GLResult!void set_data(void[] data, GLenum usage) => set_data(data.length, data.ptr, usage);
}
