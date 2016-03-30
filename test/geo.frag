#version 330 core

uniform vec4 uColor;

in vec3 vNormal;
in vec2 vUV;

out vec4 color;

void main(void) {
    color = uColor;
}
