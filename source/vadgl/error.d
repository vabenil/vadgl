module vadgl.error;

import std.traits       : EnumMembers;
import std.format       : format;

import vadgl.types;
import result;

@safe @nogc nothrow
void copy_to_buffer(string str, scope char[] buffer)
{
    size_t chars_to_copy = 
        (str.length < buffer.length)
            ? str.length : buffer.length;

    buffer[0..chars_to_copy] = str[0..chars_to_copy];
}

@safe @nogc nothrow
private string enum_to_str(T)(T f) if (is(T == enum))
{
    import std.traits   : EnumMembers;

    final switch(f)
    {
        static foreach(member; EnumMembers!T) {
            case member:
                return member.stringof;
        }
    }
}

bool is_empty(scope char[] buffer) => (buffer.length == 0 || buffer[0] == 0);

/*
    NOTE:
        - This thing allocates for strings
        - ~~There is no wayt to have nice error messages without allocations~~
        - I could make this 100% @nogc if I use `vadgl.types.GLFuncEnum` for storing
        functions and save a buffer of say 4 func enums as a function stack. For
        The extra error msg I could save it as a char[64]. It would increase stack
        memory and limit the hints I could add but it would be 100% safe.
        Though to be honest I'm not sure it would be worth the hassle
*/
// TODO: Use strings for error. It will be eaiser
// TODO: use Result!(GLError, void) instead of just GLError
// TOOD: Instead of putting all errors in a single enum.
/// I should make different enums/struct to represent different kind of errors
// and store the actual error into an union or sumType

// This is absolutely not `@nogc`. But since this is supposed to run in a single thread
// and honestly allocations here shouldn't be a problem. If they are I will worry
struct GLError
{
    // ALL here be safe!
    @safe nothrow:

    enum Flag
    {
        // 1 to 1 mappings to general OpenGL errros
        NO_ERROR = 0,
        INVALID_ENUM,
        INVALID_VALUE,
        INVALID_OPERATION,

        STACK_OVERFLOW,
        STACK_UNDERFLOW,

        // Custom errors
        // For glBind<Buffer|VertexArray>
        INVALID_TARGET,
        INVALID_BUFFER,

        // shader errors
        INVALID_SHADER,
        INVALID_SHADER_TYPE,
        SHADER_SOURCE_NOT_SET,
        SHADER_COMPILATION_ERROR,

        // program errors
        INVALID_PROGRAM,
        SHADER_ALREADY_ATTACHED,
        PROGRAM_LINK_ERROR,
        PROGRAM_VALIDATION_ERROR,

        INVALID_PARAMETER,
        UNKNOWN_ERROR = int.max
    }

    mixin injectEnum!Flag;

    /*
        TODO: Perhaps use my own static string type to avoid allocations
    */
    Flag error_flag = Flag.NO_ERROR;
    string fnc = "unknown_error";
    string msg = "";

    @nogc pure
    this(Flag error_flag, string msg="", string func_name=__FUNCTION__)
    {
        this.error_flag = error_flag;
        this.msg = msg;
        this.fnc = func_name;
    }

    @nogc pure
    this(string func, Flag error_flag = Flag.UNKNOWN_ERROR, string msg = "")
    {
        this.error_flag = error_flag;
        this.fnc = func;
        this.msg = msg;
    }

    /* GLError with_msg(string msg) => GLError(this.error_flag, msg); */

    // No sides effect here!
    pure:

    GLError append(const GLError error) const
        => GLError(this.fnc, this.error_flag, error.to_error_msg());

    GLError append(string msg) const
        => GLError(this.fnc, this.error_flag, this.msg ~ "\n" ~ msg);

    GLError append_fnc(string fnc_name = __FUNCTION__) const pure
        => GLError(this.fnc, this.error_flag, this.msg ~ "\n" ~ "\tfrom " ~ fnc_name);

    string default_msg() const
        => "[GL_ERROR]: "~this.fnc~": "~error_flag.enum_to_str();

    // TODO: maybe use toString instead
    string to_error_msg() const => this.default_msg() ~ this.msg ~ "\n";

    @nogc
    bool is_error() const => (this.error_flag != Flag.NO_ERROR);

    @nogc
    bool opCast(T: bool)() const => this.is_error();

    @nogc
    static GLError no_error() => GLError.NO_ERROR;
}

// TODO: Make an enum containing all Opengl functions
struct InternalError
{
    // All here be safe!
    @safe nothrow pure:

    mixin injectEnum!GLInternalError;

    GLInternalError error_flag;
    GLFuncEnum fnc = GLFuncEnum.UNKNOWN_GL_FUNCTION;

    this(GLInternalError error_flag) @nogc
    {
        this.error_flag = error_flag;
    }

    this(GLFuncEnum fnc, GLInternalError error_flag) @nogc
    {
        this.error_flag = error_flag;
        this.fnc = fnc;
    }

    string to_error_msg() const
        => "[GL_ERROR]: "~cast(string)this.fnc~": "~error_flag.enum_to_str();

    @nogc
    bool is_error() const => (this.error_flag != GLInternalError.NO_ERROR);

    @nogc
    bool opCast(T: bool)() const => this.is_error();
    // Technically pure since it always returns the same
    @nogc nothrow
    static InternalError no_error() => InternalError.NO_ERROR;
}

// All here is also safe, does not allocate and cannot throw!
@safe @nogc nothrow pure:

public alias GLResult = ResultPartial!GLError;

// ditto
// TODO: auto ref prob doesn't make sense here. Just `T` is better
GLResult!T glresult(T)(auto ref T val) => GLResult!T(val);
// TODO: put this inside `result.d` and check if Error is a proper
// error type
GLResult!void glresult(GLError err) => GLResult!void(err);

// ditto
GLError.Flag to_glerror_flag(GLInternalError internal) pure
{
    final switch (internal)
    {
        static foreach (flag; EnumMembers!GLInternalError)
            mixin(q{
                case GLInternalError.%1$s:
                    return GLError.Flag.%1$s;
            }.format(flag.stringof));
    }
}

GLError to_glerror(GLInternalError internal) => GLError(internal.to_glerror_flag());

GLError to_glerror(InternalError internal)
    => GLError(cast(string)internal.fnc, internal.error_flag.to_glerror_flag());

GLError.Flag to_glerror_flag(InternalError internal) => internal.to_glerror().error_flag;

GLResult!T to_glresult(T)(Result!(InternalError, T) result) if (!is(T == void))
            => GLResult!T(result.error.to_glerror(), result.value);

GLResult!T to_glresult(T)(Result!(InternalError, T) result) if (is(T == void))
            => GLResult!T(result.error.to_glerror());
