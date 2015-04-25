#version 330 core

in vec4 vColor;
in vec2 vTexCoord;

out vec4 color;

uniform sampler2D uTexture;

void main() {
  float a = texture(uTexture, vTexCoord).r;
  color = vec4(vColor.rgb, vColor.a*a);
  
  // Dump the texture contents to color:
  // color = texture(uTexture, vTexCoord);

  // Visualize the UVs:
  // vec2 debugUV = vTexCoord;
  // color = vec4(debugUV.x, debugUV.y, debugUV.x, debugUV.y);
  
  // Just use a color to make sure verts exist:
  // color = vec4(0,1,1,1);
}