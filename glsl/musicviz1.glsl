
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
uniform vec2 rms;
uniform float beat_kick;
uniform float beat_snare;
uniform float beat_hat;


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

float saturate( float x ) { return clamp( x, 0.0, 1.0 ); }

vec3 magma_quintic( float x )
{
	x = saturate( x );
	vec4 x1 = vec4( 1.0, x, x * x, x * x * x ); // 1 x x2 x3
	vec4 x2 = x1 * x1.w * x; // x4 x5 x6 x7
	return vec3(
		dot( x1.xyzw, vec4( -0.023226960, +1.087154378, -0.109964741, +6.333665763))
        + dot( x2.xy, vec2( -11.640596589, +5.337625354 ) ),
		dot( x1.xyzw, vec4( +0.010680993, +0.176613780, +1.638227448, -6.743522237))
        + dot( x2.xy, vec2( +11.426396979, -5.523236379 ) ),
		dot( x1.xyzw, vec4( -0.008260782, +2.244286052, +3.005587601, -24.279769818))
        + dot( x2.xy, vec2( +32.484310068, -12.688259703 ) ) );
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

float toLog(float value, float _min, float _max){
	float _exp = (value-_min) / (_max-_min);
	return _min * pow(_max/_min, _exp);
}


float glowdot(vec2 uv, float e) {
    float d = length(uv);
    //float d = abs(max(min(length(uv), 1.0), 0.0));
    return e * pow(d*64+1.0, -16.0);
    //return pow(1.0-d, 0.5);
}

vec3 spectrum_dots(vec2 uv) {
    vec3 col = vec3(0);
    for (float i=0; i < 1.0; i += 1.0 / 128.0) {
        vec2 mags = texture2D(fft, vec2(i/2., 0.0)).xy;
        //vec2 p = mags *1. + i;
        vec2 p = (mags - length(mags)/2.) *1. + i*0.25;
        float g =  exp_glow(abs(uv)-p, 0.002) * (mags.x + mags.y) / 2.;
        //col += magma_quintic(i) * g * length(mags);// * g * 2.0;
        col += magma_quintic(length(mags)) * g * 1.0;
    }

    return col;
}

vec3 spectrum_dots_stereo(vec2 uv) {
    vec3 col = vec3(0);
    for (float i=0; i < 1.0; i += 1.0 / 256.0) {
        vec2 mags = texture2D(fft, vec2(i/2., 0.0)).xy;
        //vec2 p = mags *1. + i;
        vec2 pl = vec2(-i*1.0, mags.x*0.25 + 0.30);
        vec2 pr = vec2( i*1.0, mags.y*0.25 + 0.30);
        //vec2 pl = vec2(-i*1.0, mags.x*0.25 - 0.45);
        //vec2 pr = vec2( i*1.0, mags.y*0.25 - 0.45);
        //float gl = smoothstep(0.005, 0., length(uv-pl));
        //float gr = smoothstep(0.005, 0., length(uv-pr));
        //float gl = exp_glow(uv-pl, 0.01);
        //float gr = exp_glow(uv-pr, 0.01);
        float gl = glowdot(uv-pl, 1.0);
        float gr = glowdot(uv-pr, 1.0);
        //col += magma_quintic(i) * g * length(mags);// * g * 2.0;
        //col += magma_quintic(i) * gl * mags.x;
        //col += magma_quintic(i) * gr * mags.y;
        col += magma_quintic(mags.x*2.) * gl * 4.0;
        col += magma_quintic(mags.y*2.) * gr * 4.0;


    }

    return col;
}

vec3 spectrum_dots2(vec2 uv) {
    vec3 col = vec3(0);
    for (float i=0; i < 1.0; i += 1.0 / 128.0) {
        float x = texture2D(fft, vec2(uv.x+ 0.3, 0.0)).x;
        float y = texture2D(fft, vec2(uv.y+ 0.3, 0.0)).y;
        vec2 mags = vec2(x, y);
        vec2 p = mags * 5.;
        float g =  exp_glow(uv-p, 0.002);
        col += magma_quintic(i) * g * 0.1;
    }

    return col;
}
void main(void) {
    float scale = 2.;
    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0);// - rms*2.001;
    uv *= scale;
    vec2 m = (mouse - 0.5) * vec2(aspect_ratio, 1.0) * scale/2.0;
    //uv -= m; // drag origin with mouse

    
    //uv.y += (rms.x + rms.y) * 2.0;
    //uv = abs(uv);

    // rotate / zoom previous frame UVs
    vec2 puv = (vertTexCoord.st - 0.5 ) * zoom * vec2(aspect_ratio, 1.0);
    
    // auto mouse wander
    //m += vec2(cos(time*0.54 + uv.y*5.321) * 0.01, // + 0.5 * rms.x,
    //         sin(time*0.38 + uv.x*1.) * 0.01);
    
    puv -= m; // drag origin with mouse
    
    float ang = 0.; //cos(time*0.001) * PI * 1.5;

    //puv *= rot(zrot); // interactive rotate
    puv *= rot(zrot+ang); // auto rotate
    puv.x /= aspect_ratio;
    puv += 0.5;
    
    //puv += rms * 20.0 - 10.;

    // screenspace wobble
    //puv.x += cos(time*0.5 + uv.y*1.321) * 0.0125 + rms.x*0.05125;
    //puv.y += sin(time*0.51 + uv.x*1.) * 0.0125 + rms.y*0.05125;
    //puv.x += cos(time*2. + uv.y*15.321) * 0.001125;
    //puv.y += sin(time*2.1 + uv.x*15.) * 0.001125;


    vec3 col = vec3(0.0);

    //float bscale = (rms.x + rms.y) * 10.0;
    float b1s = 0.1 - beat_kick*0.2;
    float b2s = 0.01 + beat_snare * 0.2;
    float b3s = 0.3 - beat_hat * 0.2;
    vec2 b1 = vec2(cos(time*1.54+beat_kick) * b1s,
             sin(time*0.38) * b1s);
    vec2 b2 = vec2(cos(time*0.54 + TAU/3.0) * b2s,
             sin(time*0.38 + TAU/3.0) * b2s);
    vec2 b3 = vec2(cos(time*0.54 + TAU*2.0/3.0) * b3s,
             sin(time*0.38 + TAU*2.0/3.0) * b3s);

    vec3 b1col = vec3(0.1, 0.1, 0.98);//hsv2rgb(vec3(0.0, 1.0, 1.0));
    col += b1col * exp_glow(uv-b1, 0.04) * beat_kick * rms.x * 3.0;
//    vec3 b2col = hsv2rgb(vec3(0.15, 1.0, 1.0));
//    col += b2col * exp_glow(uv-b2, 0.03) * beat_snare * rms.x * 20.0;
//    vec3 b3col = hsv2rgb(vec3(0.666, 1.0, 1.0));
//    col += b3col * exp_glow(uv-b3, 0.02) * beat_hat * rms.x * 20.0;
    //m += rms * 2.0;

    
    //vec2 fftuv = vec2(abs(puv.y-0.5-rms.x*0.) * 1., 0.0);
    vec2 fftuv = vec2( abs(puv.x - 0.5) , 0.0);

    float rmss = (rms.x + rms.y) * 0.0;
    //if (uv.y < rms.y -0.445 && uv.y > rms.y -0.455 ) {
    //if (uv.y < rmss -0.245 && uv.y > rmss -0.255 ) {
    if (puv.y > 0.395 && puv.y < 0.405) {
    //if (puv.y < -0.145 && puv.y > -0.155 ) {
        //vec2 fftuv = vec2(puv.x * 0.5 + 0.5, 0.0);
        float fftval = texture2D(fft, fftuv).x * 2.0;
        //col += magma_quintic(fftval);
    }

    //puv -=  texture2D(fft, fftuv).xy * 0.01;

    col += spectrum_dots_stereo(uv);
    //col += spectrum_dots(uv);
    //col += spectrum_dots2(uv);

    // dot at mouse
    vec3 dotcol = hsv2rgb(vec3(fract(time * 0.01) , 1.0, 1.0));
    //dotcol *= beat_kick;
    //col += (dotcol * exp_glow(uv-m, 0.002));
    col += (dotcol * glowdot(uv-m, 1.0));

    // axis reference dot at x = +1
    //col += vec3(1,0,0) * vec3(exp_glow(uv*scale-vec2(1.0, 0.0), 0.0125));
    // axis reference dot at y = +1
    //col += vec3(0,1,0) * vec3(exp_glow(uv*scale-vec2(0.0, 1.0), 0.0125));

    // rbg noise
    //col += vec3(hash21(uv+time), hash21(uv*1.1+time), hash21(uv*1.2+time)) * 0.15;

    //col *= crt(puv, col);
    
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
        col += tx(puv).rgb; // * 0.95;
        //col += blur(puv).rgb;
    }


    // hue rotation
    col = rgb2hsv(col);
    col.x += 0.003;
    //col.x += fract(time*0.001);
    col = hsv2rgb(col);

    //col = vec3(1);


    col = ContrastSaturationBrightness(col, contrast, saturation, brightness);

    //col = col * col;
   // col *= crt(uv, col);
    //col *= (0.97 + vec3(length(uv)*2.)*0.03); // vignette
    //col *= (1.- dot(uv, uv)); // vignette

    gl_FragColor = vec4(col, 1.);

}

