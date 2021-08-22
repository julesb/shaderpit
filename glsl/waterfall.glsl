

#define  PI 3.1415927
#define TAU 6.2831853
#define colormap inferno

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform vec2 resolution;
uniform float time;
uniform vec2 mouse;
uniform float zoom;
uniform sampler2D tex1;
uniform sampler2D fft;
uniform vec2 rms;
uniform vec2 variance;
uniform vec2 energy;

vec4 prev_frame(in vec2 p){ return texture2D(tex1, p); }

// colormap from https://www.shadertoy.com/view/WlfXRN
vec3 inferno(float t) {
    const vec3 c0 = vec3(0.00021894036911922, 0.0016510046310010, -0.019480898437091);
    const vec3 c1 = vec3(0.1065134194856116, 0.5639564367884091, 3.932712388889277);
    const vec3 c2 = vec3(11.60249308247187, -3.972853965665698, -15.9423941062914);
    const vec3 c3 = vec3(-41.70399613139459, 17.43639888205313, 44.35414519872813);
    const vec3 c4 = vec3(77.162935699427, -33.40235894210092, -81.80730925738993);
    const vec3 c5 = vec3(-71.31942824499214, 32.62606426397723, 73.20951985803202);
    const vec3 c6 = vec3(25.13112622477341, -12.24266895238567, -23.07032500287172);
    return c0+t*(c1+t*(c2+t*(c3+t*(c4+t*(c5+t*c6)))));
}

float sdSegment( in vec2 p, in vec2 a, in vec2 b ) {
    vec2 pa = p-a, ba = b-a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h );
}


vec3 energy_meter(vec2 uv) {
    float vscale = 1.0;
    vec2 origin = vec2(0.5, 1.- fwidth(uv)*20.);
    vec2 vs = abs(rms * vscale);
    vec2 l = origin - vec2(vs.x*vscale, 0.0);
    vec2 r = origin + vec2(vs.y*vscale, 0.0);
    float dl = sdSegment(uv, origin, l);
    float dr = sdSegment(uv, origin, r);
    float ll = smoothstep(0.005, 0.0, dl);
    float lr = smoothstep(0.005, 0.0, dr);
    //vec3 cl = vec3(ll);
    //vec3 cr = vec3(lr);
    return vec3(ll + lr) * 0.5;
}

vec3 variance_meter(vec2 uv) {
    float vscale = 2.0;
    vec2 origin = vec2(0.5, 1.- fwidth(uv)*40.);
    vec2 vs = variance * vscale;
    vec2 l = origin - vec2(vs.x*vscale, 0.0);
    vec2 r = origin + vec2(vs.y*vscale, 0.0);
    float dl = sdSegment(uv, origin, l);
    float dr = sdSegment(uv, origin, r);
    float ll = smoothstep(0.005, 0.0, dl);
    float lr = smoothstep(0.005, 0.0, dr);
    return vec3(ll + lr) * 0.5;
    //return cl + cr;

}

vec4 fft_os(vec2 uv) {
    return 0.5  * texture2D(fft, abs(uv))
         + 0.25 * texture2D(fft, abs(uv)-vec2(fwidth(uv.x), 0.0))
         + 0.25 * texture2D(fft, abs(uv)+vec2(fwidth(uv.x), 0.0));
}


vec3 fft_plot(vec2 uv, float fftval, float height) {
    float breathingspace = 0.9;
    float doty = 1. - height + fftval * breathingspace * height;
    vec2 dotpos = vec2(uv.x, doty);
    dotpos.y += fwidth(uv.y)*2.;
    float d = distance(uv, dotpos);
    d = uv.y < dotpos.y? 0.: d; // fill below
    //float intensity = pow(1. - d, 64.); // exp dot
    float intensity = pow(d, 0.00005); // exp dot
    //float intensity = (d < fwidth(uv.y)*1.)? 0.125: 0.0; // pixel
    float topdot = abs(uv.y - dotpos.y) < fwidth(uv.x)*1. ? 1.0: 0.0;
    //topdot *= smoothstep(0., 0.1, fftval);
    //return inferno(fftval) + vec3(topdot);
    vec3 col = colormap(fftval) * intensity + topdot;
    return col;
}

void main(void) {
    vec2 uv = gl_FragCoord.xy / resolution;
    vec2 pixsize = fwidth(uv);
    float plotheight = 0.2; 
    float plotminy = 1.0 - plotheight; 

    vec2 scrolluv = uv + vec2(0.0, fwidth(uv.y)); // vertical scroll
    vec2 fftuv = vec2((uv.x - 0.5) * 2.0, 0.0); // symmetrical centered
    fftuv.x = clamp(fftuv.x, -0.99, 0.99); // prevent sampling tex edges
    vec4 ft = fft_os(abs(fftuv));
    float fftval = (uv.x < 0.5)? ft.x : ft.y;
    
    vec3 col = vec3(0.0);
    if (uv.y > 1. - plotheight) {
        // plot
        col = fft_plot(uv, fftval, plotheight);
    }
    else {
        // waterfall
        if (uv.y > plotminy - fwidth(uv.y)*2. && uv.y < plotminy) {
            col = colormap(fftval);
        }
        else {
            col = prev_frame(scrolluv).rgb;
        }
    }

    col += energy_meter(uv);
    col += variance_meter(uv);
    // separator line
    //col += abs(uv.y - plotminy-pixsize.y) < pixsize.y ? 0.2: 0.0;
    gl_FragColor = vec4(col, 1.0);
}

