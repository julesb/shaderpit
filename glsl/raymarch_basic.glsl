#define MAX_STEPS 200
#define MAX_DIST 100.
#define SURF_DIST .001
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
vec3 eqrect(sampler2D tex, vec3 rd) {
    vec2 st = vec2(atan(rd.z, rd.x) + PI, acos(rd.y)) / vec2(2.0 * PI, PI);
    return texture2D(tex, st).rgb;
}

mat2 rot2(float a) {
    float s=sin(a), c=cos(a);
    return mat2(c, -s, s, c);
}

vec2 vmin(vec2 a, vec2 b) {
    return a.x > b.x? b : a;
}

float sdBox(vec3 p, vec3 s) {
    p = abs(p)-s;
	return length(max(p, 0.))+min(max(p.x, max(p.y, p.z)), 0.);
}

float sdSphere(vec3 p, float r) {
    return length(p) -r;
}

float sdPlane(vec3 p) {
    return p.y;
}

vec2 Box(vec3 p, vec3 s, float colid) {
    return vec2(sdBox(p, s), colid);
}

vec2 Sphere(vec3 p, float r, float colid) {
    return vec2(sdSphere(p, r), colid);
}

vec2 Plane(vec3 p, float colid) {
    return vec2(sdPlane(p), colid);
}

vec3 material(float index) {
    if (index == 0.0) {
        return vec3(0.5, 0.5, 0.5);
    }
    if (index == 1.0) {
        return vec3(0.3, 0.3, 0.9);
    }
    if (index == 2.0) {
        return vec3(0.3, 0.9, 0.3);
    }
    return vec3(0.0, 0.0, 0.0);
}

vec2 vscene(vec3 p) {
    vec2 plane = Plane(p, 0.0);
    vec2 box = Box(p - vec3(3., 1., 0.), vec3(1), 1.0);
    vec2 sphere = Sphere(p - vec3(0., 1., 0.), 1.4, 2.0);
    vec2 objects = vmin(box, sphere);
    return vmin(objects, plane);
}


vec2 vraymarch(vec3 ro, vec3 rd) {
	float dO=0.;
    float colid = 0.0;
    for(int i=0; i < MAX_STEPS; i++) {
    	vec3 p = ro + rd*dO;
        vec2 s = vscene(p);
        float dS = s.x;
        colid = s.y;

        dO += dS;
        if (dO > MAX_DIST || abs(dS) < SURF_DIST)
            break;
    }
    return vec2(dO, colid);
}

vec3 getnormal(vec3 p) {
	float d = vscene(p).x;
    vec2 e = vec2(.001, 0);
    vec3 n = d - vec3(
        vscene(p-e.xyy).x,
        vscene(p-e.yxy).x,
        vscene(p-e.yyx).x);
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

void main(void)
{
    float ar = resolution.x / resolution.y;
    vec2 uv = (vertTexCoord.st - vec2(ar*0.5, 0.5)) * -1.;

    vec3 ro = cam_pos;
    vec3 rd = cameraray(uv, ro, cam_lookat, 0.5);
    vec2 r = vraymarch(ro, rd);
    float d = r.x;
    vec3 col = material(r.y);

    if(d < MAX_DIST) {
        vec3 p = ro + rd * d;
        vec3 n = getnormal(p);// * 0.5;

        float diffuse = dot(n, normalize(vec3(1,2,3)))*.5+.5;
        col *= vec3(diffuse * 1.);
    }
    else {
        //col = eqrect(tex1, rd);
        col = vec3(.4, .6, .8) - (0.7*rd.y);
    }

    col = pow(col, vec3(gamma));	// gamma correction
    gl_FragColor = vec4(col, 1.0);
}
