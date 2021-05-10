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

vec3 intcircles(vec2 uv, float s, vec3 col) {
    float d = fract(length(uv*s));
    col *= smoothstep(0.98, 1.0, d) + smoothstep(.02, 0., d);
    return col;
}

void main(void) {
    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0) * 2.0;
    //uv = 0.1 / uv;
    vec2 m = (mouse - 0.5) * vec2(aspect_ratio, 1.0);
    uv -= m; // drag origin with mouse
    float s = 2.0; 
    vec3 col = vec3(0.0);
    
    col += grid(uv, vec3(1,1,1), s);
    col += grid(uv, vec3(.25,.25,.25), s*4);
    col += unitcircle(uv);
    col += intcircles(uv, s, vec3(0.25, 0.25, 0.25));
    
    // dot at origin
    col += vec3(exp_glow(uv, 0.025));
    // dot at x = +1
    col += vec3(1,0,0) * vec3(exp_glow(uv*s-vec2(1.0, 0.0), 0.025));
    // dot at y = +1
    col += vec3(0,1,0) * vec3(exp_glow(uv*s-vec2(0.0, 1.0), 0.025));
  

    gl_FragColor = vec4(col, 1.0);
}

