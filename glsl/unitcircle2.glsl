#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform float time;
uniform vec2 mouse;
uniform float zoom;
float PI = 3.1415927;

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

float circle(vec2 uv, float s) {
    float d = length(abs(uv*s));
    return smoothstep(0.95, 1.0, d) * smoothstep(1.05, 1.0, d);
}

vec3 unitcircle(vec2 uv) {
    vec3 col = vec3(1.);
    return col * smoothstep(0.9, 1.0, length(abs(uv*2.)))
               * smoothstep(1.1, 1.0, length(abs(uv*2.)));
}

float intcircles(vec2 uv, float s) {
    float d = fract(length(uv*s));
    return smoothstep(0.9, 1.0, d) + smoothstep(.1, 0., d);
}

float plot2d (vec2 uv) {
    float x = uv.x;
    float expr = sin(x*2.0*PI / 2.0);
    return 0.005 / length(uv - vec2(x, expr*0.5));
}

vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}


void main(void) {
    float NUM_LAYERS = 64.0;

    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0) * zoom;
    vec2 m = (mouse - 0.5) * vec2(aspect_ratio, 1.0) * zoom/2.0;
    uv -= m; // drag origin with mouse

    //uv = vec2(atan(uv.x, uv.y)/3.1415 * 0.5 + 0.5, length(uv));

    //uv.x += cos(time*1. + uv.y*1.321) * 0.25;
    //uv.y += sin(time*1.1 + uv.x*1.) * 0.25;

    //uv = 1./uv;
    float s = 2.0; 
    vec3 col = vec3(0.0);
    
    for (float i=0.; i<1.;i+=1.0/NUM_LAYERS) {
        float gridscale = 10.0 * (1.0 - fract(i + time * 0.01));
        float fade = smoothstep(10.0, 0., gridscale);
        
        //vec2 offs = sin(vec2(i * 123.41, i * 234.57+time)) * 0.02;
        float a = i * 2. * PI + time*1.5; 
        vec2 offs = vec2(sin(a) + sin(a*3.)*0.5,
                         sin(a*1.2) + sin(a*2.1))
                    * 2.0 * (1. - gridscale);
        offs *= 0.;
        vec3 layercol = sin(vec3(0.1,0.2,0.3)
                      * hash11(i*123.)
                      * 129.1+time) * 0.5 + 0.5;

        //vec3 layercol = sin(vec3(i * 123.41+time, i * 234.57+time, i*173.031+time))*0.5+0.5;
        //col += layercol * fade * intcircles(uv, i*NUM_LAYERS);
        
        float uva =  atan(uv.y-offs.y,uv.x-offs.x) / 3.1416*0.5+0.5;
        uva += fract(i*101.45 + time*0.1 * sign(sin(i*345.123)));

        // quantize hue
        uva = floor(uva * 15.) / 15.;

        //col += hsv2rgb(vec3(uva*i, 1.0, 1.0)) * 0.001;
        col +=  fade 
             * circle(uv-offs, gridscale)
             * hsv2rgb(vec3(uva, 1.0, 1.0)) ;
        //col +=  layercol *fade * circle(uv-offs, gridscale);

        //col *= layercol;
        //col += vec3(0.6, 0.6, 1.0) * fade * circle(uv-offs, gridscale);

        //col += layercol*0.1;
        //col += layercol * fade * intcircles(uv, gridscale);
        
        //col += fade * gridlayer(uv-offs, gridscale);
    }
   

    col += grid(uv, vec3(0.2,0.2,0.2), s);
    //col += grid(uv, vec3(.25,.25,.25), s*4);
    //col += unitcircle(uv);
    //col += intcircles(uv, s);
    
    // dot at origin
    //col += vec3(exp_glow(uv, 0.025));
    // dot at x = +1
    col += vec3(1,0,0) * vec3(exp_glow(uv*s-vec2(1.0, 0.0), 0.025));
    // dot at y = +1
    col += vec3(0,1,0) * vec3(exp_glow(uv*s-vec2(0.0, 1.0), 0.025));
  
    //col += vec3(0,0,1) * vec3(plot2d(uv));

    gl_FragColor = vec4(col, 1.0);
}

