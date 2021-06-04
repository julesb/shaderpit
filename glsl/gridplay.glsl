#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform float time;
uniform vec2 mouse;


float hash21(vec2 p) {
    p = fract(p * vec2(124.671, 243.563));
    p += dot(p, p + 1021.207);
    return fract(p.x * p.y);
}

float exp_glow(vec2 uv, float e) {
    return e / length(uv);
}


vec3 gridlayer(vec2 uv, float scale, float layerid) {
    // float gridscale = 10.0;   
    vec2 gv = fract(uv*scale) - 0.5;
    vec2 id = floor(uv*scale) - 0.5;
    vec3 col = vec3(0.0);
    col += 0.02 / length(gv); // dot in center of grid cell
    //col += vec3(hash21(id)) * 0.1; // color by id
    //col *= sin(vec3(0.1,0.2,0.3) * hash21(id+layerid) * 129.1 + time*1.0) * 0.5 + 0.5;
    col *= sin(vec3(0.1,0.2,0.3) * hash21(id+layerid*123.) * 129.1) * 0.5 + 0.5;
    return col;
}

void main(void) {
    float NUM_LAYERS = 8.0;
    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0) * 2.0;
    vec2 m = (mouse - 0.5) * vec2(aspect_ratio, 1.0);
    uv -= m; // drag origin with mouse
    
    vec3 col = vec3(0.0);
    
    for (float i=0.; i<1.;i+=1.0/NUM_LAYERS) {
        float gridscale = 20.0 * (1.0 - fract(i + time * 0.1));
        float fade = smoothstep(20., 10., gridscale);
        vec2 offs = sin(vec2(i * 123.41, i * 234.57)) * 0.2;
                  // * smoothstep() ;
        col += fade * gridlayer(uv, gridscale, i*NUM_LAYERS);
        //col += fade * gridlayer(uv-offs, gridscale);
    }

    //col *= 0.3; 
    // dot at origin
    //col += vec3(exp_glow(uv, 0.01));

    gl_FragColor = vec4(col, 1.0);
}

