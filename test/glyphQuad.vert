#version 330 core

uniform mat4 uMVP;

in vec3 aPosition;
in vec4 aColor;
in vec2 aTexCoord;

out vec4 vColor;
out vec2 vTexCoord;

void main() { 

    gl_Position = uMVP * vec4( aPosition , 1.0 );

    vColor    = aColor;
    vTexCoord = aTexCoord;
}