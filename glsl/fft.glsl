
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
uniform sampler2D fft;
uniform float rms;

vec4 tx(in vec2 p){ return texture2D(tex1, p); }

float saturate( float x ) { return clamp( x, 0.0, 1.0 ); }

// palettes from https://www.shadertoy.com/view/XtGGzG
vec3 viridis_quintic( float x )
{
	x = saturate( x );
	vec4 x1 = vec4( 1.0, x, x * x, x * x * x ); // 1 x x2 x3
	vec4 x2 = x1 * x1.w * x; // x4 x5 x6 x7
	return vec3(
		dot( x1.xyzw, vec4( +0.280268003, -0.143510503, +2.225793877, -14.815088879 ) )
        + dot( x2.xy, vec2( +25.212752309, -11.772589584 ) ),
		dot( x1.xyzw, vec4( -0.002117546, +1.617109353, -1.909305070, +2.701152864 ) )
        + dot( x2.xy, vec2( -1.685288385, +0.178738871 ) ),
		dot( x1.xyzw, vec4( +0.300805501, +2.614650302, -12.019139090, +28.933559110 ) )
        + dot( x2.xy, vec2( -33.491294770, +13.762053843 ) ) );
}

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

vec3 plasma_quintic( float x )
{
	x = saturate( x );
	vec4 x1 = vec4( 1.0, x, x * x, x * x * x ); // 1 x x2 x3
	vec4 x2 = x1 * x1.w * x; // x4 x5 x6 x7
	return vec3(
		dot( x1.xyzw, vec4( +0.063861086, +1.992659096, -1.023901152, -0.490832805 ) )
        + dot( x2.xy, vec2( +1.308442123, -0.914547012 ) ),
		dot( x1.xyzw, vec4( +0.049718590, -0.791144343, +2.892305078, +0.811726816 ) )
        + dot( x2.xy, vec2( -4.686502417, +2.717794514 ) ),
		dot( x1.xyzw, vec4( +0.513275779, +1.580255060, -5.164414457, +4.559573646 ) )
        + dot( x2.xy, vec2( -1.916810682, +0.570638854 ) ) );
}



void main(void) {
    vec2 uv = gl_FragCoord.xy / resolution;
    vec3 col = vec3(0.0);
    
    vec2 scrolluv = uv + vec2(0.0, fwidth(uv.y));
    col = tx(scrolluv).rgb;

    vec2 fftuv = vec2(uv.x * 0.25, 0.0);
    //vec2 fftuv = vec2(abs(uv.x - 0.5) * 1.0, 0.0); // symmetrical
    float fftval = texture2D(fft, fftuv).x;
    
    vec3 fftcolor = plasma_quintic(fftval);
    if (uv.y > 1.0 - fwidth(uv.y)) {
        col = fftcolor * fftval * 2.;
    } 

    gl_FragColor = vec4(col, 1.0);
}

