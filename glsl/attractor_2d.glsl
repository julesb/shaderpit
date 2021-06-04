#define AC vec2(1.5)
#define BD vec2(1.5)
#define ITER 500.0

varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform float time;
uniform vec2 mouse;
uniform float zoom;


void main(void) {
    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0) * zoom;
    uv -= (mouse - 0.5) * vec2(aspect_ratio, 1.0) * zoom/2.0;
    uv *= 6.0;

    vec3 col = vec3(0.);
    vec2 m = vec2(time * 0.051, time *0.05);
    vec2 p = vec2(0.0);    

    // Attractor:
    // xn+1 = sin(A*yn) - cos(B*xn)
	// yn+1 = sin(C*xn) - cos(D*yn)

    for (float i=1.0; i<2.0; i+=1.0/ITER){
        p = sin(m+AC*p.yx) - cos(m+BD*p);
        col += vec3(0.8, 0.2, 0.05)
             * smoothstep(vec3(0.005, 0.058, 0.12)*i, vec3(0.),
                          vec3(distance(uv, p)));
    }

    gl_FragColor = vec4(col, 0.5);
}

