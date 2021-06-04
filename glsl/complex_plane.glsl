#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform float time;
uniform vec2 mouse;
uniform float zoom;
float PI = 3.1415927;

#define cx_mul(a, b) vec2(a.x*b.x-a.y*b.y, a.x*b.y+a.y*b.x)
//#define cx_div(a, b) (cx_mul(a , cx_inv(b)))
#define cx_div(a, b) vec2(((a.x*b.x+a.y*b.y)/(b.x*b.x+b.y*b.y)),((a.y*b.x-a.x*b.y)/(b.x*b.x+b.y*b.y)))
#define cx_modulus(a) length(a)
#define cx_conj(a) vec2(a.x,-a.y)
#define cx_arg(a) atan2(a.y,a.x)
#define cx_sin(a) vec2(sin(a.x) * cosh(a.y), cos(a.x) * sinh(a.y))
#define cx_cos(a) vec2(cos(a.x) * cosh(a.y), -sin(a.x) * sinh(a.y))
vec2 cx_mobius(vec2 a) {
    vec2 c1 = a - vec2(1.0,0.0);
    vec2 c2 = a + vec2(1.0,0.0);
    return cx_div(c1, c2);
}

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

float plot2d (vec2 uv) {
    float x = uv.x;
    float expr = sin(x*2.0*PI / 2.0);
    return 0.005 / length(uv - vec2(x, expr*0.5));
}


void main(void) {
    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0) * zoom;
    //uv = 0.05 / uv;
    vec2 m = (mouse - 0.5) * vec2(aspect_ratio, 1.0) * zoom/2.0;
    uv = cx_mobius(cx_div(m, uv));
    //uv -= m; // drag origin with mouse

    //uv -= vec2(sin(time * 0.2), sin(time * 0.221));

    float s = 2.0; 
    vec3 col = vec3(0.0);
    
    col += grid(uv, vec3(1,1,1), s);
    //col += grid(uv, vec3(.25,.25,.25), s*4);
    col += unitcircle(uv);
    col += intcircles(uv, s, vec3(0.25, 0.25, 0.25));
    
    // dot at origin
    col += vec3(exp_glow(uv, 0.025));
    // dot at x = +1
    col += vec3(1,0,0) * vec3(exp_glow(uv*s-vec2(1.0, 0.0), 0.025));
    // dot at y = +1
    col += vec3(0,1,0) * vec3(exp_glow(uv*s-vec2(0.0, 1.0), 0.025));
  
    col += vec3(0,0,1) * vec3(plot2d(uv));

    gl_FragColor = vec4(col, 1.0);
}

