//#extension GL_EXT_gpu_shader4 : enable
#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

varying vec4 vertTexCoord;
uniform float time;
uniform float mousex;
uniform float mousey;
uniform float aspect_ratio;

float imagefunc (float A, float B, float T) {
	return 0.5 * (1.0 + (cos(A * 15.0 + T) * sin(B * 15.0 + T)));
}

float imagefunc2 (float A, float B, float T) {
	int scale = 1280;
    int size = 128;
    int m = scale / size;
    A += scale;
    B += scale;

    if ((int(A * m) % 2 == 0 && int(B * m) % 2 == 1)
    ||  (int(A * m) % 2 == 1 && int(B * m) % 2 == 0)) {
        //if (int(T + A*m * B*m ) % 2 == 0) {
        if (true) {
        //if (sin(T + A*m ) > 0) {
            return 1.0;
        }
        else {
            return 0.0;
        }
    }
    else {
        return 0.0;
    }
}


void main(void) {
    vec2 q = vertTexCoord.st;
    float mx = mousex * aspect_ratio;
    float r = imagefunc2(q.s - mx + sin(time * 0.5), q.t - mousey, time * 1.2); 
    float g = imagefunc2(q.s - mx - sin(time * 0.5), q.t - mousey, time * 1.2); 
    float b = imagefunc2(q.s - mx, q.t - mousey + sin(time * 0.5), time * 1.2); 


    r *= imagefunc(q.s - mx, q.t - mousey, time * 1.02); 
    g *= imagefunc(q.s - mx, q.t - mousey, time * -1.12); 
    b *= imagefunc(q.s - mx, q.t - mousey, time * -1.13); 


    gl_FragColor = vec4(r, g, b, 1.0);
}

