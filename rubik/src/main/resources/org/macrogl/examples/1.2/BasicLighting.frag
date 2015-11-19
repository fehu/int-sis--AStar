#version 120

#ifdef GL_ES
precision mediump float;
#endif

varying vec3 fragColor;
varying vec3 fragNormal;

uniform mat4 worldTransform;
uniform vec3 lightColor;
uniform float ambient;
uniform float diffuse;


vec3 lightF = vec3( 10, 0, 0);
vec3 lightB = vec3(-10, 0, 0);
vec3 lightR = vec3( 0,  10, 0);
vec3 lightL = vec3( 0, -10, 0);
vec3 lightU = vec3( 0, 0,  10);
vec3 lightD = vec3( 0, 0, -10);

void main(void) {
  vec3 lights[6];
  lights[0] = lightF;
  lights[1] = lightB;
  lights[2] = lightR;
  lights[3] = lightL;
  lights[4] = lightU;
  lights[5] = lightD;

  vec3 transformedNormal = normalize(mat3(worldTransform) * fragNormal);
  vec3 ambientColor = ambient * lightColor;
  vec3 diffuseColor = vec3(0, 0, 0);


  for (int index = 0; index < lights.length(); index++){
    float diffuseFactor = max(0, dot(transformedNormal, -lights[index]));
    diffuseColor = diffuseColor + diffuseFactor * diffuse * lightColor;
  }

  gl_FragColor = vec4(fragColor * (diffuseColor + ambientColor), 1.0);
}
