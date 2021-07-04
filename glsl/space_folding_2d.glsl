varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform float time;
uniform vec2 mouse_l;
uniform vec2 mouse_r;
uniform vec2 mouse_c;
uniform float zoom;
uniform float zrot;

float PI = 3.1415927;

mat2 rot2(float a) {
    float s=sin(a), c=cos(a);
    return mat2(c, -s, s, c);
}

float exp_glow(vec2 uv, float e) {
    return e / length(uv);
}

float grid(vec2 uv, vec3 col,  float s) {
    return length(smoothstep(0.48, 0.5, abs(fract(uv*s) - 0.5)));
}

float unitcircle(vec2 uv) {
    return smoothstep(0.98, 1.0, length(abs(uv*2.)))
         * smoothstep(1.02, 1.0, length(abs(uv*2.)));
}

vec3 loopfold(vec2 uv, vec2 p) {
    const int iters = 12;
    vec3 col = vec3(0);

    for(int i=0; i < iters; i++)  {
        float a = i * PI*2.0 / iters;
        vec2 n = vec2(1,0) * rot2(a);
        p *= rot2(a);
        //vec2 p1 = p * rot2(a);
        float d = dot(uv-p, n);
        uv -= n * min(d, 0.0) * 2.0;
        

        col += vec3(1,1,1) * 0.5 * smoothstep(0.05, 0.0, abs(d));
    }

    return col;
}


void main(void) {
    vec2 armap = vec2(aspect_ratio, 1.0) * zoom;
    vec2 uv = (vertTexCoord.st - 0.5) * armap;
    vec2 m1 = (mouse_l - 0.5) * armap * 0.5;
    vec2 m2 = (mouse_r - 0.5) * armap * 0.5;
    vec2 m3 = (mouse_c - 0.5) * armap * 0.5;

    uv -= m3; // drag origin with center mouse
    float s = 2.0; 
    float ang = atan(m2.y, m2.x);

    vec2 n = vec2(cos(ang), sin(ang));
    float d = dot(uv-m1+m3, n); // drag plane of reflection with left mouse
    uv -= n * min(d, 0.0) * 2.0;

    vec3 col = vec3(0.0);
    col += grid(uv, vec3(1,1,1), s);
    col += unitcircle(uv);
    
    col.rg += uv* 0.5;

    // plane of reflection
    col += vec3(1,1,0) * smoothstep(0.01, 0.0, abs(d));

    col += loopfold(uv*zoom, m1);

    // dot at origin
    col += vec3(exp_glow(uv, 0.025));
    // dot at x = +1
    col += vec3(1,0,0) * vec3(exp_glow(uv*s-vec2(1.0, 0.0), 0.025));
    // dot at y = +1
    col += vec3(0,1,0) * vec3(exp_glow(uv*s-vec2(0.0, 1.0), 0.025));
    
    // dot at mouse
    col += vec3(1,1,0) * vec3(exp_glow(uv-m1+m3, 0.025));
  

    gl_FragColor = vec4(col, 1.0);
}

