#version 330 core

uniform mat4 uMVP;
uniform float uXOffset;

in vec3 aVertex;
in vec4 aColor;
in vec2 aTexCoord;

out vec4 vColor;
out vec2 vTexCoord;

void main() { 

    vec4 finalVertex = vec4( aVertex.x + uXOffset, aVertex.y, aVertex.z, 1.0);
    gl_Position = uMVP * finalVertex;

    vColor    = aColor;
    vTexCoord = aTexCoord;
}