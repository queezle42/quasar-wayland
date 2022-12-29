#version 100
//#extension GL_OES_EGL_image_external : require

precision mediump float;
uniform vec2 window;

uniform sampler2D sampler;
//uniform samplerExternalOES sampler;

void main() {
  gl_FragColor = texture2D(sampler, vec2(gl_FragCoord.x / window.x, gl_FragCoord.y / window.y)).gbra;
}
