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
#### Even easier!
Using abstractions `Shader` and `Program` abstractions you can create a progam
like so:
```d
string vertex_source = ... // vertex source
string frag_source = ... //  fragment source

Shader vert_sh, frag_sh;
// Shaders created with error handling and everything!
vert_sh = Shader.from_src("vertex", Shader.Type.VERTEX, vertex_source).throw_on_error();
frag_sh = Shader.from_src("frag", Shader.Type.FRAGMENT, frag_source).throw_on_error();

Program program = Program.create_program("program").throw_on_error();

// Compile, prepare, attach and link shaders to program
if (auto res = program.prepare_and_attach(&vert_sh, &frag_sh)) {
    // Log error message
    stderr.writeln(res.error.to_error_msg());
    // print info log for shaders if there's an error
    stderr.writeln(vert_sh.get_info_log().throw_on_error());
    stderr.writeln(frag_sh.get_info_log().throw_on_error());
    assert(0);
}
// Use Program
program.use().throw_on_error();
```

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
