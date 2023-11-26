module vadgl.types;

// TODO: Be less dependant on this
import bindbc.opengl;

// TODO: Check that all Args are string
private template buildEnumBodyString(Args...)
{
    static if (Args.length == 1)
        enum string buildEnumBodyString = Args[0]~"\n";
    else
        enum string buildEnumBodyString = Args[0]~", \n"~buildEnumBodyString!(Args[1..$]);
}

// For some reason this doesn't work
/* private template FilterArgs(args...) */
/* { */
/*     import std.meta; */
/*     import std.range    : iota; */

/*     alias FilterArgs_ = AliasSeq!(); */
/*     static foreach (i; iota(0, args.length, 2)) { */
/*         static if (args[i+1]) { */
/*             FilterArgs = AliasSeq!(FilterArgs, args[i]); */
/*         } */
/*     } */
/* } */

private template FilterArgs(Args...) if (Args.length % 2 == 0)
{
    import std.meta;
    static if (Args.length == 2) {
        static if (Args[1]) {
            alias FilterArgs = AliasSeq!(Args[0]);
        }
        else {
            alias FilterArgs = AliasSeq!();
        }
    }
    else {
        static if (Args[1]) {
            alias FilterArgs = AliasSeq!(Args[0], FilterArgs!(Args[2..$]));
        }
        else {
            alias FilterArgs = FilterArgs!(Args[2..$]);
        }
    }

}

private mixin template ConditionalEnum(string enum_name, Args...)
{
    mixin("enum "~enum_name~" {\n "~
            buildEnumBodyString!(FilterArgs!Args)~
        "};\n"
    );
}

mixin ConditionalEnum!("GLType",
    "BYTE = GL_BYTE",               glSupport >= GLSupport.gl11,
    "UBYTE = GL_UNSIGNED_BYTE",     glSupport >= GLSupport.gl11,
    "SHORT = GL_SHORT",             glSupport >= GLSupport.gl11,
    "USHORT = GL_UNSIGNED_SHORT",   glSupport >= GLSupport.gl11,

    "INT = GL_INT",                 glSupport >= GLSupport.gl11,
    "UINT = GL_UNSIGNED_INT",       glSupport >= GLSupport.gl11,

    "FLOAT = GL_FLOAT",             glSupport >= GLSupport.gl11,
    "DOUBLE = GL_DOUBLE",           glSupport >= GLSupport.gl11,

    "VEC2 = GL_FLOAT_VEC2",         glSupport >= GLSupport.gl20,
    "VEC3 = GL_FLOAT_VEC3",         glSupport >= GLSupport.gl20,
    "VEC4 = GL_FLOAT_VEC4",         glSupport >= GLSupport.gl20,

    "IVEC2 = GL_INT_VEC2",          glSupport >= GLSupport.gl20,
    "IVEC3 = GL_INT_VEC3",          glSupport >= GLSupport.gl20,
    "IVEC4 = GL_INT_VEC4",          glSupport >= GLSupport.gl20,

    // OpenGL version 3.0 or above
    "HALF_FLOAT = GL_HALF_FLOAT",   glSupport >= GLSupport.gl30,
    "UVEC2 = GL_UNSIGNED_INT_VEC2", glSupport >= GLSupport.gl30,
    "UVEC3 = GL_UNSIGNED_INT_VEC3", glSupport >= GLSupport.gl30,
    "UVEC4 = GL_UNSIGNED_INT_VEC4", glSupport >= GLSupport.gl30,
);


// This is what the expression above should compile to
/* enum GLType */
/* { */
/*     // Basic GL types */
/*     //>> Accepted by `glVertexAttribPointer` and `glVertexAttribIPointer` */
/*     BYTE = GL_BYTE, */
/*     UBYTE = GL_UNSIGNED_BYTE, */

/*     SHORT = GL_SHORT, */
/*     USHORT = GL_UNSIGNED_SHORT, */

/*     INT = GL_INT, */
/*     UINT = GL_UNSIGNED_INT, */
/*     //<< end acceppted by `glVertexAttribIPointer` */

/*     //>> Accepted by `glVertexAttribPointer` and `glVertexAttribLPointer` */
/*     FLOAT = GL_FLOAT, */
/*     HALF_FLOAT = GL_HALF_FLOAT, // requires OpenGL version >=3.0 */
/*     DOUBLE = GL_DOUBLE, */
/*     //<< End acceppted by `glVertexAttribPointer` and `glVertexAttribLPointer` */

/*     // Vector types */
/*     VEC2 = GL_FLOAT_VEC2, */
/*     VEC3 = GL_FLOAT_VEC3, */
/*     VEC4 = GL_FLOAT_VEC4, */

/*     IVEC2 = GL_INT_VEC2, */
/*     IVEC3 = GL_INT_VEC3, */
/*     IVEC4 = GL_INT_VEC4, */

/*     // Add a bool vectors here */

/*     // Requires gl30 */
/*     UVEC2 = GL_UNSIGNED_INT_VEC2, */
/*     UVEC3 = GL_UNSIGNED_INT_VEC3, */
/*     UVEC4 = GL_UNSIGNED_INT_VEC4, */

/*     // TODO: Add matrix types */
/* } */

// TODO: maybe make module
enum GLFuncEnum : string
{
    UNKNOWN_GL_FUNCTION = "unknown_function", // General OpenGL error for any function
    GL_CREATE_SHADER = "glCreateShader",
    GL_SHADER_SOURCE = "glShaderSource",
    GL_COMPILE_SHADER = "glCompileShader",

    GL_ATTACH_SHADER = "glAttachShader",
    GL_LINK_PROGRAM = "glLinkProgram",

    GL_BIND_VERTEX_ARRAY = "glBindVertexArray",
    GL_BUFFER_DATA = "glBufferData",
    GL_BIND_BUFFER = "glBindBuffer",

    GL_VERTEX_ATTRIB_POINTER = "glVertexAttribPointer",
    GL_VERTEX_ATTRIB_I_POINTER = "glVertexAttribIPointer",

    GL_ENABLE_VERTEX_ATTRIB_ARRAY = "glEnableVertexAttribArray",
    GL_ENABLE_DISABLE_ATTRIB_ARRAY = "glDisableVertexAttrbArray",


    CREATE_SHADER = "create_shader",
    SHADER_COMPILE = "",

    PROGRAM_ATTACH = "",
    PROGRAM_LINK = "",
    PROGREM_VALIDATE = "",

    BIND = "",
}

// This should all work with OpenGL version 1.1 or above so no need for condition
// TODO: make this support multiple OpenGL versions
enum GLInternalError
{
    NO_ERROR                       = GL_NO_ERROR,
    INVALID_ENUM                   = GL_INVALID_ENUM,
    INVALID_VALUE                  = GL_INVALID_VALUE,
    INVALID_OPERATION              = GL_INVALID_OPERATION,
    STACK_OVERFLOW                 = GL_STACK_OVERFLOW,
    STACK_UNDERFLOW                = GL_STACK_UNDERFLOW,
}

/* enum GLParam { */
/*     SHADER_TYPE               =  GL_SHADER_TYPE, */
/*     DELETE_STATUS             =  GL_DELETE_STATUS, */
/*     COMPILE_STATUS            =  GL_COMPILE_STATUS, */
/*     INFO_LOG_LENGTH           =  GL_INFO_LOG_LENGTH, */
/*     SHADER_SOURCE_LENGTH      =  GL_SHADER_SOURCE_LENGTH, */

/*     LINK_STATUS               =  GL_LINK_STATUS, */
/*     VALIDATE_STATUS           =  GL_VALIDATE_STATUS, */
/*     ATTACHED_SHADERS          =  GL_ATTACHED_SHADERS, */

/*     // Only in OpenGL >=  3.2 */
/*     GEOMETRY_VERTICES_OUT     =  GL_GEOMETRY_VERTICES_OUT, */
/*     GEOMETRY_INPUT_TYPE       =  GL_GEOMETRY_INPUT_TYPE, */
/*     GEOMETRY_OUTPUT_TYPE      =  GL_GEOMETRY_OUTPUT_TYPE, */
/* } */

mixin ConditionalEnum!("GLParam",
    "SHADER_TYPE = GL_SHADER_TYPE",                       glSupport >= GLSupport.gl20,
    "DELETE_STATUS = GL_DELETE_STATUS",                   glSupport >= GLSupport.gl20,
    "COMPILE_STATUS = GL_COMPILE_STATUS",                 glSupport >= GLSupport.gl20,
    "INFO_LOG_LENGTH = GL_INFO_LOG_LENGTH",               glSupport >= GLSupport.gl20,
    "SHADER_SOURCE_LENGTH = GL_SHADER_SOURCE_LENGTH",     glSupport >= GLSupport.gl20,

    "LINK_STATUS = GL_LINK_STATUS",                       glSupport >= GLSupport.gl20,
    "VALIDATE_STATUS = GL_VALIDATE_STATUS",               glSupport >= GLSupport.gl20,
    "ATTACHED_SHADERS = GL_ATTACHED_SHADERS",             glSupport >= GLSupport.gl20,

    // Only in OpenGL > = 3.2
    "GEOMETRY_VERTICES_OUT = GL_GEOMETRY_VERTICES_OUT",   glSupport >= GLSupport.gl32,
    "GEOMETRY_INPUT_TYPE = GL_GEOMETRY_INPUT_TYPE",       glSupport >= GLSupport.gl32,
    "GEOMETRY_OUTPUT_TYPE = GL_GEOMETRY_OUTPUT_TYPE",     glSupport >= GLSupport.gl32,
);
