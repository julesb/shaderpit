
// 0 = clip
// 1 = mirror
#define BOUNDARY_MODE 0

#define  PI 3.1415927
#define TAU 6.2831853

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform vec2 resolution;
uniform float time;
uniform vec2 mouse;
uniform float zoom;
uniform float zrot;

uniform sampler2D tex1;

mat2 rot(float a) {
    float s=sin(a), c=cos(a);
    return mat2(c, -s, s, c);
}

float hash21(vec2 p) {
    p = fract(p * vec2(124.671, 243.563));
    p += dot(p, p + 1021.207);
    return fract(p.x * p.y);
}

float hash11(float p) {
    return fract((sin(p)*0.5+0.5) * 249.8631);
}

vec3 hash23(vec2 p) {
    return vec3(hash21(p+1456.78), hash21(p+1203.45), hash21(p+2134.67));
}

float exp_glow(vec2 uv, float e) {
    return e / length(uv);
}

vec3 grid(vec2 uv, vec3 col,  float s) {
    vec2 gv = fract(uv*s) - 0.5;
    col *= length(smoothstep(0.48, 0.5, abs(gv)));
    return col;
}

vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

vec3 rgb2hsv(vec3 c)
{
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));
    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

vec4 tx(in vec2 p){ return texture2D(tex1, p); }

vec3 blur(in vec2 p){
    vec3 e = vec3(1, 0, -1);
    vec2 px = 1./resolution;
    vec3 res = vec3(0.0);
    // corners
    res += tx(p + e.xx*px).rgb
         + tx(p + e.xz*px).rgb
         + tx(p + e.zx*px).rgb
         + tx(p + e.zz*px).rgb;
    // sides.
    res += (  tx(p + e.xy*px).rgb
            + tx(p + e.yx*px).rgb
            + tx(p + e.yz*px).rgb
            + tx(p + e.zy*px).rgb)*2.;
    // center
    res += tx(p + e.yy*px).rgb*4.;
    return res/16.;
}

void main(void) {
    float scale = 2.;
    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0);
    uv *= scale;
    vec2 m = (mouse - 0.5) * vec2(aspect_ratio, 1.0) * scale/2.0;
    //uv -= m; // drag origin with mouse

    //uv = abs(uv);

    // rotate / zoom previous frame UVs
    vec2 puv = (vertTexCoord.st - 0.5 ) * zoom * vec2(aspect_ratio, 1.0);
    
    float ang = cos(time*0.005) * TAU;

    puv *= rot(ang); // auto rotate
    //puv *= rot(zrot); // interactive rotate
    puv.x /= aspect_ratio;
    puv += 0.5;
    
    // screenspace wobble
    //puv.x += cos(time*1. + uv.y*15.321) * 0.002125;
    //puv.y += sin(time*1.1 + uv.x*15.) * 0.002125;

    // auto mouse wander
    m = vec2(cos(time*0.74 + uv.y*5.321) * 0.4,
             sin(time*0.68 + uv.x*1.) * 0.4);

    vec3 col = vec3(0.0);

    vec3 dotcol = hsv2rgb(vec3(ang * 60., 1.0, 1.0));
    // dot at mouse
   col += (dotcol * exp_glow(uv-m, 0.0025));
    //col += hsv2rgb(vec3(time*0.2, 1., 1.)) * exp_glow(uv-m, 0.0015);

    // axis reference dot at x = +1
    //col += vec3(1,0,0) * vec3(exp_glow(uv*scale-vec2(1.0, 0.0), 0.0125));
    // axis reference dot at y = +1
    //col += vec3(0,1,0) * vec3(exp_glow(uv*scale-vec2(0.0, 1.0), 0.0125));
    
    if (BOUNDARY_MODE == 0) {
        // clip puv
        if (puv.x > 0. && puv.x < 1. && puv.y > 0. && puv.y < 1.) {
            //col += blur(puv).rgb;// * 0.95;
            col += tx(puv).rgb;// * 0.9;
        }
    }
    else {
        // mirror puv
        puv = abs(puv);
        if (puv.x > 1.) puv.x = 1. - puv.x;
        if (puv.y > 1.) puv.y = 1. - puv.y;
        col += tx(puv).rgb * 0.95;

    }

    //col = (col - 0.125) * 2.0;

    //col = col*col;

    // noise
    //col += hash23(uv * time*0.1) * 0.1;
    //col += hash21(uv+time) * 0.01;
    //col = vec3(hash21(uv+time), hash21(uv*1.1+time), hash21(uv*1.2+time)) * 1.00;
    
    //col *= (0.97 + vec3(length(uv)*2.)*0.03); // vignette
    //col *= (1.- dot(uv, uv)); // vignette

    gl_FragColor = vec4(col, 1.0);
}

