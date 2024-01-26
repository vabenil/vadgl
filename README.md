### Warning
This library is still in alpha states and will be actively changing. If you have
any change in mind put it as an issue here.

### About
D abstraction over OpenGL. Planned to be used with any OpenGL bindings. None of
the functions in this library throw. If compiled with version
`VADGL_DisableChecks` or in `release` version this library does not allocate and
therefore is `@nogc`.

### Features
- `@nothrow` error handling system
- Does not allocate (in `release` and `VADGL_DisableChecks`)
- Debugging all OpenGL calls with (`VADGL_DebugGLCalls`)
- Works with any bindings

### Why?
OpenGL is not known for having the best error handling. This library provides a
type safe D-like interface for OpenGL with it's own error handling model for better
debugging plus abstractions over Shaders, VAOs, VBOs, Attributes and Uniforms.

### How can I use this?


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
