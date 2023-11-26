/+
Unittest shit here
+/
/* import std.stdio; */
/* import std.string; */
/* import core.stdc.stdlib : exit; */

/* import bindbc.sdl; */
/* import bindbc.opengl; */

/* import vadgl; */
/* import result; */


/* enum size_t WIN_WIDTH = 600; */
/* enum size_t WIN_HEIGHT = 400; */

/* struct GState */
/* { */
/*     SDL_Window* win = null; */
/*     SDL_GLContext ctx = null; */

/*     bool is_running = true; */

/*     Program program; */

/*     uint vao; */
/*     uint vbo; */
/*     uint ebo; */
/* } */

/* struct Vertex */
/* { */
/*     float[3] pos; */
/* } */

/* GState state; */

/* static immutable uint[6] ebo_indices = [0, 1, 2, 2, 3, 0]; */

/* /1* static immutable Vertex[4] vertices = [ *1/ */
/* /1*     Vertex([-0.2, -0.2, 1.0], [1, 1, 1, 1]), *1/ */
/* /1*     Vertex([ 0.2, -0.2, 1.0], [1, 1, 1, 1]), *1/ */
/* /1*     Vertex([ 0.2,  0.2, 1.0], [1, 1, 1, 1]), *1/ */
/* /1*     Vertex([-0.2,  0.2, 1.0], [1, 1, 1, 1]), *1/ */
/* /1* ]; *1/ */
/* static immutable float[12] vertices = [ */
/*     -0.2, -0.2, 1.0, */
/*      0.2, -0.2, 1.0, */
/*      0.2,  0.2, 1.0, */
/*     -0.2,  0.2, 1.0, */
/* ]; */

/* static immutable size_t vertex_size = float.sizeof * 3; */

/* @trusted */
/* static void set_opengl_attributes(int mayor_v, int minor_v) */
/* { */
/*     // Use OpenGL 3.2 */
/*     SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, mayor_v); */
/*     SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, minor_v); */
/*     SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE); */

/*     SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8); */
/*     SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8); */
/*     SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8); */
/*     SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8); */
/*     SDL_GL_SetAttribute(SDL_GL_BUFFER_SIZE, 32); */

/*     // double buffered window */
/*     SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1); */
/* } */

/* static void load_opengl() */
/* { */
/*     GLSupport ret = loadOpenGL(); */

/*     assert(ret >= GLSupport.gl32); */
/*     set_opengl_attributes(3, 2); */
/* } */

/* static void create_window() */
/* { */
/*     state.win = SDL_CreateWindow( */
/*         "vadgl test", */
/*         SDL_WINDOWPOS_CENTERED, */
/*         SDL_WINDOWPOS_CENTERED, */
/*         WIN_WIDTH, WIN_HEIGHT, SDL_WINDOW_OPENGL */
/*     ); */

/*     assert(state.win); */

/*     state.ctx = SDL_GL_CreateContext(state.win); */
/*     assert(state.ctx); */

/*     assert(SDL_Init(SDL_INIT_VIDEO) >= 0); */
/* } */

/* static void gl_init() */
/* { */
/*     static immutable string vert_shader = q{ */
/*         #version 330 core */
/*         layout(location=0) in vec3 pos; */

/*         void main() { */
/*             gl_Position = vec4(pos, 1); */
/*         } */
/*     }; */
/*     static immutable string frag_shader = q{ */
/*         #version 330 core */

/*         out vec4 color; */

/*         void main() { */
/*             color = vec4(1, 1, 1, 1); */
/*         } */
/*     }; */

/*     glClearColor(.8f, .0f, .8f, 1.0f); */

/*     Shader vert_sh, frag_sh; */
/*     vert_sh = Shader.from_src("vert", Shader.Type.VERTEX, vert_shader).unwrap(); */
/*     frag_sh = Shader.from_src("frag", Shader.Type.FRAGMENT, frag_shader).unwrap(); */

/*     state.program = Program.create_program("program").unwrap(); */
/*     state.program.prepare_and_attach(&vert_sh, &frag_sh) */
/*         .unwrap_or_else!( */
/*             (res) { */
/*                 string error_msg; */
/*                 writeln(res.error.to_error_msg); */

/*                 vert_sh.get_info_log(error_msg); */
/*                 if (error_msg) writeln(error_msg); */

/*                 frag_sh.get_info_log(error_msg); */
/*                 if (error_msg) writeln(error_msg); */

/*                 exit(1); */
/*             } */
/*         ); */

/*     writefln("Program(%d) was successfully created and prepared", state.program.id); */

/*     state.program.use(); */

/*     gl_wrap!glGenVertexArrays(1, &state.vao).unwrap(); */
/*     gl_wrap!glGenBuffers(1, &state.vbo).unwrap(); */
/*     gl_wrap!glGenBuffers(1, &state.ebo).unwrap(); */

/*     // Send data */
/*     glBindBuffer(GL_ARRAY_BUFFER, state.vbo); */
/*     glBufferData(GL_ARRAY_BUFFER, vertices.length * Vertex.sizeof, vertices.ptr, GL_STATIC_DRAW); */

/*     glBindBuffer(GL_ARRAY_BUFFER, state.ebo); */
/*     glBufferData(GL_ARRAY_BUFFER, ebo_indices.length * uint.sizeof, ebo_indices.ptr, GL_STATIC_DRAW); */

/*     glBindBuffer(GL_ARRAY_BUFFER, 0); */

/*     glBindVertexArray(state.vao); */
/*     glBindBuffer(GL_ARRAY_BUFFER, state.vbo); */
/*     glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, state.ebo); */

/*     glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, vertex_size, cast(void *)0); */
/*     glEnableVertexAttribArray(0); */
/* } */

/* void init() */
/* { */
/*     create_window(); */
/*     load_opengl(); */
/*     gl_init(); */
/* } */

/* unittest */
/* { */
/*     init(); */

/*     while(state.is_running) { */
/*         // event loop */
/*         SDL_Event e; */
/*         while(SDL_PollEvent(&e)) { */
/*             if (e.type == SDL_QUIT) */
/*                 state.is_running = false; */
/*             else if (e.type == SDL_KEYDOWN) { */
/*                 if (e.key.keysym.sym == SDLK_q) */
/*                     state.is_running = false; */
/*             } */
/*         } */

/*         glUseProgram(state.program.id); */
/*         glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); */
/*         glEnable(GL_BLEND); */
/*         /1* glEnable(GL_DEPTH_TEST); *1/ */

/*         glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); */

/*         glBindVertexArray(state.vao); */
/*         glBindBuffer(GL_ARRAY_BUFFER, state.vbo); */
/*         glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, state.ebo); */

/*         glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, null); */

/*         SDL_GL_SwapWindow(state.win); */
/*     } */
/* } */
