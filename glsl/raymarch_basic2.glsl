#define MAX_STEPS 200
#define MAX_DIST 100.
#define SURF_DIST .0001
#define PI 3.141592653

uniform float time;
uniform float framecount;

uniform vec2 resolution;
uniform float aspect_ratio;

uniform vec3 cam_pos;
uniform vec3 cam_lookat;
uniform float cam_fov;
uniform vec2 mouse;

uniform float ray_hit_epsilon;
uniform float gamma;

uniform sampler2D tex1;

varying vec4 vertColor;
varying vec4 vertTexCoord;

// https://www.shadertoy.com/view/WtGXDD

vec3 to_spherical(vec3 v) {
    v = v.xzy;
    float r = length(v);
    float lon = acos(v.x / sqrt(v.x * v.x + v.y * v.y)) * (v.y < 0.0 ? -1.0 : 1.0);
    float lat = acos(v.z / r);
    return vec3(lon,lat,r);
}

mat2 rot2(float a) {
    float s=sin(a), c=cos(a);
    return mat2(c, -s, s, c);
}

vec3 op_rep(vec3 p, vec3 spread) {
    return mod(p+spread*0.5, spread) - spread*0.5;
}

vec2 op_intersect(vec2 a, vec2 b) {
    float d = max(a.x, b.x);
    if (d <= b.x)
        return vec2(d,b.y);
    else
        return vec2(d,a.y);
}

float sdBox(vec3 p, vec3 s) {
    p = abs(p)-s;
	return length(max(p, 0.))+min(max(p.x, max(p.y, p.z)), 0.);
}

float sdSphere(vec3 p, float r) {
    return length(p) -r;
}

float scene(vec3 p) {
    //p = fract(p);
    vec3 prep = op_rep(p, vec3(5., 5., 5.));
    float box = sdBox(prep - vec3(0., 0., 0.), vec3(1));
    float sphere = sdSphere(prep - vec3(0., 0., 0.), 1.5);
    float objects = max(box*1.0, sphere*1.0);
    //float objects = min(box, sphere);
    float plane = p.y;
    return min(objects, plane);
}

vec2 raymarch(vec3 ro, vec3 rd) {
	float dO=0.;
    float g = 0.; 
    for(int i=0; i < MAX_STEPS; i++) {
        g += 1.;
    	vec3 p = ro + rd*dO;
        float dS = scene(p);
        dO += dS;
        if (dO > MAX_DIST || abs(dS) < SURF_DIST)
            break;
    }
    return vec2(dO, g);
}

vec3 getnormal(vec3 p) {
	float d = scene(p);
    vec2 e = vec2(.001, 0);
    vec3 n = d - vec3(
        scene(p-e.xyy),
        scene(p-e.yxy),
        scene(p-e.yyx));
    return normalize(n);
}

vec3 cameraray(vec2 uv, vec3 p, vec3 l, float z) {
    vec3 f = normalize(l-p),                    // cam forward dir
         r = normalize(cross(vec3(0,1,0), f)),  // cam right dir
         u = cross(f,r),                        // cam up dir
         c = f*z,                               // center of image plane
         i = c + uv.x*r + uv.y*u,               // intersection image plane
         d = normalize(i);
    return d;
}

vec3 sky(sampler2D tex, vec3 rd) {
    vec3 spher = to_spherical(rd);
    vec2 st = spher.xy / vec2(2.0*PI, PI*2.0);
    st.y = 1.0 - st.y;
    return texture2D(tex, st).rgb;

}

void main(void)
{
    float ar = resolution.x / resolution.y;
    vec2 uv = (vertTexCoord.st - vec2(ar*0.5, 0.5)) * -1.;
    vec3 ro = cam_pos;
    
    vec3 rd = cameraray(uv, ro, cam_lookat, 0.5);
    vec3 col = vec3(0);
   
    vec2 d = raymarch(ro, rd);
    //float d = raymarch(ro, rd);

    vec3 glow = vec3(d.y / MAX_STEPS);

    if(d.x < MAX_DIST) {
        vec3 p = ro + rd * d.x;
        vec3 n = getnormal(p) * 0.5;
        
        col = abs(n);
        //glow *= col;

        float diffuse = dot(n, normalize(vec3(1,2,3)))*.5+.5;
        col *= vec3(diffuse * 1.);
        // col *= p.y * 0.3;
        //col += vec3(.1, .1, 1.) * smoothstep(0., 0.1, p.y);
        //col += vec3(0., 0., 1.) * smoothstep(0., 0.001, p.y);
    }
    else {
        col = sky(tex1, rd);
        //col = vec3(.2, .3, .7);
    }
    
    //col += glow;

    //col = texture2D(tex1, vertTexCoord.st).rgb;
    col = pow(col, vec3(gamma));	// gamma correction
    gl_FragColor = vec4(col, 1.0);
}
