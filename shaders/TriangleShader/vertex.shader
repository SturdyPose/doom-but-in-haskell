#version 460 core

layout(location = 0) in vec3 vPosition;
// layout(location = 1) in vec4 vcolor;
// layout(location = 2) in mat4 transform;

void main()
{
  gl_Position = vec4(vPosition, 1.0);
}