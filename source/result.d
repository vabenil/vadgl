module result;

import std.stdio;
import std.format           : format;
import core.attribute       : mustuse;
import std.traits           : ReturnType, isSomeFunction;

// TODO: check if is enum 
mixin template injectEnum(EnumType)
{
    private import std.format       : format;
    private import std.traits       : EnumMembers;

    static foreach (member; EnumMembers!EnumType)
        mixin(q{
            enum typeof(this) %1$s = typeof(this)(%2$s.%1$s);
        }.format(member.stringof, EnumType.stringof));
}

private enum isFuncValidA(alias fnc, R) = __traits(compiles, fnc(R.init));
private enum isFuncValidB(alias fnc, R) =
                __traits(compiles, fnc(R.init, string.init, size_t.init));

// T must have a default constructor
template Result(ErrorType, T)
{
    @safe @nogc nothrow:

    @mustuse
    struct Result
    {
        // use GLErrorInfo
        ErrorType error;

        static if (!is(T == void)) {
            T value;

            this(ErrorType error, T value)
            {
                this.error = error;
                this.value = value;
            }

            // TODO: somehow set to NO_ERROR
            this(T value)
            {
                this.error = ErrorType.no_error();
                this.value = value;
            }
        }

        this(ErrorType error) { this.error = error; }


        bool is_error() const => this.error.is_error();
        bool opCast(T: bool)() const => is_error();
    }
}

template ResultPartial(ErrorType)
{
    alias ResultPartial(T) = Result!(ErrorType, T);
}

T unwrap_or_else(alias fnc, E, T)(Result!(E, T) result)
if (isSomeFunction!fnc && isFuncValidA!(fnc, Result!(E, T)))
{
    if (result.error.is_error()) {
        static if (is(ReturnType!fnc == T))
            return fnc(result);
        else
            fnc(result);
    }

    static if (!is(T == void))
        return result.value;
}

T unwrap_or_else(alias fnc, E, T)(Result!(E, T) result)
if (!isSomeFunction!fnc && isFuncValidA!(fnc, Result!(E, T)))
{
    return unwrap_or_else!(fnc!(Result!(E, T)), E, T)(result);
}

T unwrap_or_else(alias fnc, E, T)(Result!(E, T) result, string file = __FILE__, size_t line = __LINE__)
if (isSomeFunction!fnc && isFuncValidB!(fnc, typeof(result)))
{
    if (result.error.is_error()) {
        static if (is(ReturnType!fnc == T))
            return fnc(result, file, line);
        else
            fnc(result, file, line);
    }
    static if (!is(T == void))
        return result.value;
}

T unwrap_or_else(alias fnc, E, T)(Result!(E, T) result, string file = __FILE__, size_t line = __LINE__)
if (!isSomeFunction!fnc && isFuncValidB!(fnc, typeof(result)))
{
    return unwrap_or_else!(fnc!(typeof(result)), E, T)(result, file, line);
}

void throw_on_error_(E, T)(Result!(E, T) res, string file = __FILE__, size_t line = __LINE__)
{
    throw new Exception(res.error.to_error_msg(), file, line);
}

T throw_on_error(E, T)(Result!(E, T) result, string file = __FILE__, size_t line = __LINE__)
    => result.unwrap_or_else!(throw_on_error_!(E, T), E, T)(file, line);

// Maybe use core.stdc.assert
/* T unwrap_or_assert(T)(GLResult!T res, string file = __FILE__, size_t line = __LINE__) */
/* { */
/*     import core.stdc.assert_; */
/*     import std.format           : format; */
/*     import std.string           : toStringz; */

/*     return unwrap_or_else!( */
/*         (res, file, line) => assert(res.error) */
/*     )(res, file, line); */
/* } */

template match(handlers...)
{
    import std.sumtype;

    auto match(E, T)(ref Result!(E, T) result)
    {
        static if (is(T == void))
            alias ResSum = SumType!(E);
        else
            alias ResSum = SumType!(E, T);

        if (result.is_error())
            return std.sumtype.match!(handlers)(ResSum(result.error));
        else
            return std.sumtype.match!(handlers)(ResSum(result.value));
    }
}
