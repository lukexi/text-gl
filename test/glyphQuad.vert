#version 330 core

uniform mat4 uMVP;

in vec3 aVertex;
in vec4 aColor;
in vec2 aTexCoord;

out vec4 vColor;
out vec2 vTexCoord;

void main() { 

    gl_Position = uMVP * vec4( aVertex , 1.0 );

    vColor    = aColor;
    vTexCoord = aTexCoord;
}