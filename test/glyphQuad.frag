#version 330 core

uniform sampler2D uTexture;
uniform vec3 uColor;

in vec2 vTexCoord;

out vec4 color;

void main() {

  float a = texture(uTexture, vTexCoord).r;
  color = vec4(uColor, a);
  
  // Dump the texture contents to color:
  // color = texture(uTexture, vTexCoord);

  // Visualize the UVs:
  // vec2 debugUV = vTexCoord;
  // color = vec4(debugUV.x, debugUV.y, debugUV.x, debugUV.y);
  
  // Just use a color to make sure verts exist:
  // color = vec4(0,1,1,1);
}
