#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform float time;
uniform vec2 mouse;


float hash21(vec2 p) {
    p = fract(p * vec2(124.671, 243.563));
    p += dot(p, p + 1021.207);
    return fract(p.x * p.y);
}

float exp_glow(vec2 uv, float e) {
    return e / length(uv);
}

vec3 grid(vec2 uv, vec3 col,  float s) {
    vec2 gv = fract(uv*s) - 0.5;
    col *= length(smoothstep(0.48, 0.5, abs(gv)));
    return col;
}

vec3 unitcircle(vec2 uv) {
    vec3 col = vec3(1.);
    return col * smoothstep(0.98, 1.0, length(abs(uv*2.)))
               * smoothstep(1.02, 1.0, length(abs(uv*2.)));
}

void main(void) {
    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0) * 2.0;
    vec2 m = (mouse - 0.5) * vec2(aspect_ratio, 1.0);
    uv -= m; // drag origin with mouse
    
    vec3 col = vec3(0.0);
    
    col += grid(uv, vec3(1,1,1), 2.0);
    col += grid(uv, vec3(.5,.5,.5), 10.0);
    col += unitcircle(uv);
    // dot at origin
    col += vec3(exp_glow(uv, 0.01));

    gl_FragColor = vec4(col, 1.0);
}

