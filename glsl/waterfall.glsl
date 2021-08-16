

#define  PI 3.1415927
#define TAU 6.2831853

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform vec2 resolution;
uniform float time;
uniform vec2 mouse;
uniform float zoom;
uniform sampler2D tex1;
uniform sampler2D fft;
uniform float rms;

vec4 prev_frame(in vec2 p){ return texture2D(tex1, p); }
float saturate( float x ) { return clamp( x, 0.0, 1.0 ); }

// palettes from https://www.shadertoy.com/view/XtGGzG
vec3 inferno_quintic( float x )
{
	x = saturate( x );
	vec4 x1 = vec4( 1.0, x, x * x, x * x * x ); // 1 x x2 x3
	vec4 x2 = x1 * x1.w * x; // x4 x5 x6 x7
	return vec3(
		dot( x1.xyzw, vec4( -0.027780558, +1.228188385, +0.278906882, +3.892783760 ) )
        + dot( x2.xy, vec2( -8.490712758, +4.069046086 ) ),
		dot( x1.xyzw, vec4( +0.014065206, +0.015360518, +1.605395918, -4.821108251 ) )
        + dot( x2.xy, vec2( +8.389314011, -4.193858954 ) ),
		dot( x1.xyzw, vec4( -0.019628385, +3.122510347, -5.893222355, +2.798380308 ) )
        + dot( x2.xy, vec2( -3.608884658, +4.324996022 ) ) );
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
    float d = distance(uv, dotpos);
    //d = uv.y < dotpos.y? 0.: d; // fill below
    //float intensity = pow(1. - d, 64.); // exp dot
    float intensity = (d < fwidth(uv.y)*1.)? 0.125: 0.0; // pixel
    float topdot = abs(uv.y - dotpos.y) < fwidth(uv.x)*1. ? 1.0: 0.0;
    return inferno_quintic(fftval) * intensity + topdot;
}

void main(void) {
    vec2 uv = gl_FragCoord.xy / resolution;
    vec2 pixsize = fwidth(uv);
    float plotheight = 0.2; 
    float plotminy = 1.0 - plotheight; 

    vec2 scrolluv = uv + vec2(0.0, fwidth(uv.y)); // vertical scroll
    vec2 fftuv = vec2((uv.x - 0.5) * 2.0, 0.0); // symmetrical centered
    vec4 ft = fft_os(fftuv);
    float fftval = (uv.x < 0.5)? ft.x : ft.y;
    
    vec3 col = vec3(0.0);
    if (uv.y > 1. - plotheight) {
        // plot
        col = fft_plot(uv, fftval, plotheight);
    }
    else {
        // waterfall
        if (uv.y > plotminy - fwidth(uv.y) && uv.y < plotminy) {
            col = inferno_quintic(fftval);
        }
        else {
            col = prev_frame(scrolluv).rgb;
        }
    }

    // separator line
    col += abs(uv.y - plotminy-pixsize.y) < pixsize.y ? 0.1: 0.0;

    gl_FragColor = vec4(col, 1.0);
}

