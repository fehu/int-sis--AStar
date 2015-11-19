#version 120

attribute vec3 position;
attribute vec3 normal;
attribute vec3 color;

uniform mat4 projection;
uniform mat4 viewTransform;
uniform mat4 worldTransform;

varying vec3 fragColor;
varying vec3 fragNormal;


void main(void) {
  gl_Position = vec4(position, 1.0);
  gl_Position = projection * viewTransform * worldTransform * vec4(position, 1.0);
  fragColor  = color;
  fragNormal = normal;
}
