### Warning
This library is still in alpha states and will be actively changing. If you have
any change in mind put it as an issue here.

### About
D abstraction over OpenGL. Planned to be used with any OpenGL bindings. None of
the functions in this library throw. If compiled with version
`VADGL_DisableChecks` or in `release` version ~~all~~ most of the functions in
the library do not allocate (The ones that do work with strings and there are
`const(char*)` overloads that are `@nogc`).

### Features
- `@nothrow` error handling system
- Does not allocate (in `release` and `VADGL_DisableChecks`)
- Debugging all OpenGL calls with (`VADGL_DebugGLCalls`)
- Works with any bindings
- Shader, Program, VBO, VAO, Attribute and Uniform abstractions

### Why?
OpenGL is not known for having the best error handling. This library provides
function overrides, a type safe D-like interface for OpenGL with it's own error
handling model for better debugging plus abstractions over Shaders, VAOs, VBOs,
Attributes and Uniforms.

### How can I use this?
OpenGL functions in **VADGL** use snake_case instead of camelCase. For example the
**VADGL** equivalent of `glCreateShader` is `gl_create_shader`.
So programs like:
```d
uint vert_sh = glCreateShader(GL_VERTEX_SHADER);
assert(glGetError() == GL_NO_ERROR);
```
becomes:
```d
uint vert_hs = gl_create_shader(Shader.Type.VERTEX).throw_on_error();
```
or for `@nothrow`
```d
uint vert_hs =
        gl_create_shader(Shader.Type.VERTEX)
            .unwrap_or_else!((result) => assert(0, result.error.to_error_msg));
```
With the additional benefit that if an error happens. You will get an error
message like this:
```
[GL_ERROR]: glCreateShader: INVALID_ENUM
```
#### Using abstractions
Using `Shader` and `Program` abstractions you can create a Program
like so:
```d
string vertex_source = ... // vertex source
string frag_source = ... //  fragment source

Shader vert_sh, frag_sh;
vert_sh = Shader.from_src("vertex", Shader.Type.VERTEX, vertex_source) .throw_on_error();
frag_sh = Shader.from_src("frag", Shader.Type.FRAGMENT, frag_source) .throw_on_error();

program = Program.create_program("program").throw_on_error();

// Compile shaders
foreach (shader; [&vert_sh, &frag_sh]) {
    shader.compile().unwrap_or_else((res) {
        stderr.writeln(res.error.to_error_msg()); // Print compilation error
        // Get compilation error message if any
        stderr.writeln(vert_sh.get_info_log().throw_on_error);
        assert(0);
    });
}
program.attach(vert_sh, frag_sh).throw_on_error(); // attach
program.link().throw_on_error(); // link
program.validate().throw_on_error(); // validate

program.use().throw_on_error();
```
With this we create and compile shaders `vert_sh` and `frag_sh`, then attach
them to the program `program`, and finally link and validate the program.

All with error checking! Here take a look at a shader compilation error using
this code:
```
[GL_ERROR]: vadgl.gl3.Shader.compile: SHADER_COMPILATION_ERROR: Failed to compile `vertex` shader
        HINT: Run get_info_log() to get extra information

Error on "vertex" shader
0:36(33): error: `model_pos' undeclared
0:36(27): error: cannot construct `float' from a non-numeric data type
0:37(33): error: `model_pos' undeclared
0:37(27): error: cannot construct `float' from a non-numeric data type

core.exception.AssertError@example.d(15): Assertion failure
```
And just like that without any extra code we get so much information of what
went wrong.

### Extra examples (On construction):

### TODO:
- [PRIORITY]: Write testing framework (Should be easy, we have a way to log GL)
- [PRIORITY]: Write documentation
- [PRIORITY]: ~~Maybe~~ use `GLInternalError` for functions that map directly to OpenGL
- Use GL types
- Rework `result.d`, keep API the same if possible
- Add more version blocks **[important]**
- Add abstraction over textures
- Maybe add runtime checks
- Add more OpenGL functions
