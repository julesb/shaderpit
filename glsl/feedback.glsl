
// 0 = clip
// 1 = mirror
#define BOUNDARY_MODE 1

#define  PI 3.1415927
#define TAU 6.2831853

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform vec2 resolution;
uniform float time;
uniform vec2 mouse;
uniform float zoom;
uniform float zrot;
uniform float brightness;
uniform float contrast;
uniform float saturation;

uniform sampler2D tex1;

vec3 ContrastSaturationBrightness(vec3 color, float con, float sat, float brt) {
    const float AvgLumR = 0.5;
    const float AvgLumG = 0.5;
    const float AvgLumB = 0.5;
    const vec3 LumCoeff = vec3(0.2125, 0.7154, 0.0721);
    vec3 AvgLumin  = vec3(AvgLumR, AvgLumG, AvgLumB);
    vec3 brtColor  = color * brt;
    vec3 intensity = vec3(dot(brtColor, LumCoeff));
    vec3 satColor  = mix(intensity, brtColor, sat);
    vec3 conColor  = mix(AvgLumin, satColor, con);
    return conColor;
}


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


float random (vec2 st) {
    return fract(sin(dot(st.xy, vec2(12.9898,78.233)))* 43758.5453123);
}


vec3 crt(vec2 uv, vec3 col) {
    float density = 2.0;
    float opacityScanline = 0.5;
    float opacityNoise = .5;
    float flickering = 0.03;

    float count = resolution.y * density;
    vec2 sl = vec2(sin(uv.y * count), cos(uv.y * count)) * 0.5 + 0.5;
    //sl = sl * 0.8 + 0.2;
	vec3 scanlines = vec3(sl.x, sl.y, sl.x);

    col += col * scanlines * opacityScanline;
    col += col * vec3(random(uv*time)) * opacityNoise;
    col += col * sin(110.0*time) * flickering;


    return col;
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
    
    float ang = cos(time*0.01) * PI * 0.5;

    //puv *= rot(zrot); // interactive rotate
    puv *= rot(zrot+ang); // auto rotate
    puv.x /= aspect_ratio;
    puv += 0.5;
    
    // screenspace wobble
    puv.x += cos(time*1. + uv.y*15.321) * 0.001125;
    puv.y += sin(time*1.1 + uv.x*15.) * 0.001125;

    // auto mouse wander
    m = vec2(cos(time*0.54 + uv.y*5.321) * 0.3,
             sin(time*0.38 + uv.x*1.) * 0.3);

    vec3 col = vec3(0.0);

    // dot at mouse
    vec3 dotcol = hsv2rgb(vec3(fract(time * 0.01) , 1.0, 1.0));
   col += (dotcol * exp_glow(uv-m, 0.01));

    // axis reference dot at x = +1
    //col += vec3(1,0,0) * vec3(exp_glow(uv*scale-vec2(1.0, 0.0), 0.0125));
    // axis reference dot at y = +1
    //col += vec3(0,1,0) * vec3(exp_glow(uv*scale-vec2(0.0, 1.0), 0.0125));

    // rbg noise
    //col += vec3(hash21(uv+time), hash21(uv*1.1+time), hash21(uv*1.2+time)) * 0.15;

    col *= crt(puv, col);
    
    if (BOUNDARY_MODE == 0) {
        // clip puv
        if (puv.x > 0. && puv.x < 1. && puv.y > 0. && puv.y < 1.) {
            //col += blur(puv).rgb; // * 0.90;
            col += tx(puv).rgb;// * 0.9;
        }
    }
    else {
        // mirror puv
        puv = abs(puv);
        if (puv.x > 1.) puv.x = 1. - puv.x;
        if (puv.y > 1.) puv.y = 1. - puv.y;
        //col += tx(puv).rgb; // * 0.95;
        col += blur(puv).rgb;
    }


    // hue rotation
    col = rgb2hsv(col);
    col.x += 0.01;
    //col.x += fract(time*0.001);
    col = hsv2rgb(col);

    //col = vec3(1);


    col = ContrastSaturationBrightness(col, contrast, saturation, brightness);

    //col = col * col;
    //col *= crt(uv, col);
    //col *= (0.97 + vec3(length(uv)*2.)*0.03); // vignette
    col *= (1.- dot(uv, uv)); // vignette

    gl_FragColor = vec4(col, 0.75);
}

