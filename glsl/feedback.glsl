#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform vec2 resolution;
uniform float time;
uniform vec2 mouse;
uniform float zoom;
uniform float zrot;

uniform sampler2D tex1;
float PI = 3.1415927;
float TAU = 2. * PI;

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
    float NUM_LAYERS = 64.0;

    vec2 nb = 1.0 / resolution;

    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0);
    uv *= scale;
    vec2 m = (mouse - 0.5) * vec2(aspect_ratio, 1.0) * scale/2.0;
    //uv -= m; // drag origin with mouse

    // rotate / zoom previous frame UVs
    vec2 puv = (vertTexCoord.st - 0.5 ) * zoom * vec2(aspect_ratio, 1.0);
    puv *= rot(zrot + sin(time*0.01)*TAU);
    //puv *= rot(zrot);
    puv.x /= aspect_ratio;
    puv += 0.5;
    
    //uv.x += cos(time*1. + uv.y*1.321) * 0.25;
    //uv.y += sin(time*1.1 + uv.x*1.) * 0.25;
    m = vec2(cos(time*0.5 + uv.y*1.321) * 0.4,
             sin(time*0.41 + uv.x*1.) * 0.4);

    vec3 col = vec3(0.0);
    

    //col += grid(uv, vec3(0.2,0.2,0.2), s);

    // dot at origin
    col += hsv2rgb(vec3(time*0.25, 1., 1.)) * exp_glow(uv-m, 0.0015);
    
    // dot at x = +1
    //col += vec3(1,0,0) * vec3(exp_glow(uv*scale-vec2(1.0, 0.0), 0.0125));
    // dot at y = +1
    //col += vec3(0,1,0) * vec3(exp_glow(uv*scale-vec2(0.0, 1.0), 0.0125));
    
    if (puv.x > 0. && puv.x < 1. && puv.y > 0. && puv.y < 1.) { 
        //col += blur(puv) * 0.99;
        col += tx(puv).rgb * 0.99;
    }
    
    //col += hash21(uv) * 0.001;

    gl_FragColor = vec4(col, 1.0);
}

