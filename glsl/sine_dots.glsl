#define AC vec2(1.5)
#define BD vec2(1.5)
#define ITER 1000.0
#define PI 3.1415926
#define TAU (2.*PI)

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform float time;
uniform vec2 mouse;
uniform float zoom;

float exp_glow(vec2 uv, float e) {
    return e / length(uv);
}


void main(void) {
    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0) * zoom;
    uv -= (mouse - 0.5) * vec2(aspect_ratio, 1.0) * zoom/2.0;
    //uv *= 6.0;
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
        col += smoothstep(0.015, 0., length(uv - p));
        //col += exp_glow(uv - p, 0.0005);
    }

    col *= vec3(0.2, 1.0,0.3);
    gl_FragColor = vec4(col, 1.0);
}

