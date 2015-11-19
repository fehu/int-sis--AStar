#version 120

#ifdef GL_ES
precision mediump float;
#endif

varying vec3 fragColor;
varying vec3 fragNormal;

uniform mat4 worldTransform;
uniform vec3 lightColor;
uniform vec3 lightDirection;
uniform float ambient;
uniform float diffuse;

void main(void) {

vec3 transformedNormal = normalize(mat3(worldTransform) * fragNormal);
  float diffuseFactor = max(0, dot(transformedNormal, -lightDirection));

  vec3 diffuseColor = diffuseFactor * diffuse * lightColor;
  vec3 ambientColor = ambient * lightColor;

  gl_FragColor = vec4(fragColor * (diffuseColor + ambientColor), 1.0);
}
