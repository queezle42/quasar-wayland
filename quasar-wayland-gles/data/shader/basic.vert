#version 100

attribute vec2 pos;
uniform float scale;

void main() {
  gl_Position = vec4(pos * scale, 0, 1);
}
