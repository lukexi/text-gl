#version 330 core

uniform mat4 uMVP;

uniform float uXOffset;
uniform float uYOffset;

in vec2 aPosition;
in vec2 aTexCoord;

out vec2 vTexCoord;

void main() { 

    gl_Position = uMVP * vec4(aPosition.x + uXOffset, aPosition.y + uYOffset, 0.0, 1.0);

    vTexCoord = aTexCoord;
}
