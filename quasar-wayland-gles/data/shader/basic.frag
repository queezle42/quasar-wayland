#version 100

precision mediump float;
uniform vec2 window;

void main() {
  gl_FragColor = vec4(gl_FragCoord.x / window.x, 0, gl_FragCoord.y / window.y, 1);
}
