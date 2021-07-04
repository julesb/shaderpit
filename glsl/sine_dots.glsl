#define AC vec2(1.5)
#define BD vec2(1.5)
#define ITER 50.0
#define PI 3.1415926
#define TAU (2.*PI)

#define LOOP_COUNT_I 64
#define LOOP_COUNT_F 64.0

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform float time;
uniform vec2 mouse;
uniform float zoom;
uniform vec2 resolution;
uniform sampler2D tex1;

float exp_glow(vec2 uv, float e) {
    return e / length(uv);
}




vec3 radialblur(vec2 uv, vec2 l) {
    //uv = -1.0 + 2.0*fragCoord/iResolution.xy;
    //l = -1.0 + 2.0*iMouse.xy/iResolution.xy;

    vec3 col = vec3(0.0);
    vec2 d = (l-uv)/LOOP_COUNT_F;
    float w = 1.0;
    vec2  s = uv;
    for( int i=0; i<LOOP_COUNT_I; i++ )
    {
        col += w*smoothstep( 0.0, 1.0, texture2D(tex1, s * 0.3).rgb);
                                    //texture(iChannel0, s * 0.3).xyz);
        w *= 0.99;
        s += d;
    }
    col = col * 2.0 / LOOP_COUNT_F;

	return col;
}


void main(void) {
    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0) * zoom;
    //vec2 buv = 0.0 + 2.0*vertTexCoord.xy*vec2(aspect_ratio, 1.0);
    vec2 buv = uv * 6.;
    buv.x /= aspect_ratio;
    buv -= 1.666;

    vec2 muv = (mouse - 0.5) * vec2(aspect_ratio, 1.0) * zoom/2.0;
    //uv -= muv;
    //uv *= 6.;
    muv *= 6.;
    //uv = 1. / (uv*uv);
    //uv.x += sin(time+uv.y*5.)*0.1;

    vec3 col = vec3(0.);
    vec2 m = vec2(0.49, 0.51);
    //vec2 m = vec2(1., 1.);
    vec2 p = vec2(0.0);    

    //time *= 2.;

    for (float i=0.0; i<1.0; i+=1.0/ITER){
        p   = sin(m*i*101. + time)
            + sin(m*i*212. + time*1.) * 0.5
            + sin(m*i*323. + time*1.) * 0.25
            + sin(m*i*434. + time*1.) * 0.125
            ;
        p *= 0.2;
        col += smoothstep(0.01125, 0., length(uv - p));
        //col += exp_glow(uv - p, 0.0005);
    }

    col *= vec3(0.2, 1.0,0.3);
    //vec2 buv = uv / vec2(aspect_ratio, 1.) * 0.5;
    col += radialblur(buv, muv ) * 0.5;
    gl_FragColor = vec4(col, 1.0);
}

