varying vec4 vertTexCoord;
uniform float aspect_ratio;
uniform float time;
uniform vec2 mouse;
uniform float zoom;
uniform float brightness;
uniform float contrast;
uniform float saturation;

float PI = 3.1415927;

#define NUM_LAYERS 16.
#define ITER 23

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



vec4 tex(vec3 p)
{
    float t = 0.2*time+78.;
    vec4 o = vec4(p.xyz,3.*sin(t*.1));
    vec4 c = vec4 (1., .9, .1, .15);

    vec4 dec = c + vec4(.06 * cos(t*.1), 0, 0, .14 * cos(t *.23));
    for (int i=0 ; i++ < ITER;) o.xzyw = abs(o/dot(o,o)- dec);
    return o;
}

void main(void)
{
    vec2 uv = (vertTexCoord.st - 0.5) * vec2(aspect_ratio, 1.0) * zoom;
    vec2 m = (mouse - 0.5) * vec2(aspect_ratio, 1.0) * zoom/2.0;
    float t = time * .1;
    vec3 col = vec3(0);   
    
    uv -= m;
    
	for(float i=0.; i<=1.; i+=1./NUM_LAYERS) {
        float d = fract(i+t); // depth
        float s = mix(5.,.5,d); // scale
        float f = d * smoothstep(1.,.9, d); //fade
        col+= tex(vec3(uv*s,i*4.)).xyz*f;
    }
    
    col/=NUM_LAYERS;
    col*=vec3(2.0,1.0,2.);
   	col=pow(col,vec3(.5 ));  

    col = ContrastSaturationBrightness(col, contrast, saturation, brightness);

    gl_FragColor = vec4(col,1.0);
}

