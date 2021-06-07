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
varying vec4 vertTexCoord;

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

float sdRoundBox( vec3 p, vec3 b, float r )
{
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0) - r;
}

float sdBox(vec3 p, vec3 s) {
    p = abs(p)-s;
	return length(max(p, 0.))+min(max(p.x, max(p.y, p.z)), 0.);
}

float sdSphere(vec3 p, float r) {
    return length(p) -r;
}

float scene(vec3 p) {
    vec3 bpos = p;
    //vec3 bpos = p - vec3(0., 1., 0.);
    bpos.xz *= rot2(time *0.5);
    
    float box = sdRoundBox(bpos, vec3(1), 0.15);
    //float box = sdBox(p - vec3(3., 1., 0.), vec3(1));
    float sphere = sdSphere(p - vec3(3., 0., 0.), 1.);
    float objects = min(box, sphere);
    return objects;
    //float plane = p.y;
    //return min(objects, plane);
}

float raymarch(vec3 ro, vec3 rd, float side) {
	float dO=0.;
    for(int i=0; i < MAX_STEPS; i++) {
    	vec3 p = ro + rd*dO;
        float dS = scene(p) * side;
        dO += dS;
        if (dO > MAX_DIST || abs(dS) < SURF_DIST)
            break;
    }
    return dO;
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
    vec2 st = spher.xy / vec2(2.0*PI, PI*1.0);
    st.y = 1.0 - st.y;
    return texture2D(tex, st).rgb;

}

void main(void)
{
    float ar = resolution.x / resolution.y;
    vec2 uv = (vertTexCoord.st - vec2(ar*0.5, 0.5)) * -1.;

    vec3 ro = cam_pos;
    float IOR = 1.5;

    vec3 rd = cameraray(uv, ro, cam_lookat, 0.5);
    vec3 col = vec3(0);
   
    float d = raymarch(ro, rd, 1.0);


    if(d < MAX_DIST) {
        vec3 p = ro + rd * d;
        vec3 n = getnormal(p);// * 0.5;
        vec3 rdIn = refract(rd, n, 1.0/IOR);
        
        vec3 pEnter = p - n * SURF_DIST * 2.0;
        float dIn = raymarch(pEnter, rdIn, -1.0);
        vec3 pExit = pEnter + rdIn * dIn;
        vec3 nExit = -getnormal(pExit);
        vec3 rdOut = refract(rdIn, nExit, IOR);
        if (dot(rdOut, rdOut) == 0.0) {
            rdOut = reflect(rdIn, nExit);
        }
        col = sky(tex1, rdOut);
        //col += abs(n) * 0.25;
        
        float diffuse = dot(n, normalize(vec3(1,2,3)))*.5+.5;
        col *= vec3(diffuse * 1.);
    }
    else {
        col = sky(tex1, rd);
        //col = vec3(.2, .3, .7);
    }

    //col = texture2D(tex1, vertTexCoord.st).rgb;
    col = pow(col, vec3(gamma));
    gl_FragColor = vec4(col, 1.0);
}
