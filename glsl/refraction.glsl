#define MAX_STEPS 200
#define MAX_DIST 50.
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

float sdTorus( vec3 p, vec2 t )
{
  vec2 q = vec2(length(p.xz)-t.x,p.y);
  return length(q)-t.y;
}

float sdTriPrism( vec3 p, vec2 h )
{
  vec3 q = abs(p);
  return max(q.z-h.y,max(q.x*0.866025+p.y*0.5,-p.y)-h.x*0.5);
}

float sdOctahedron( vec3 p, float s)
{
  p = abs(p);
  return (p.x+p.y+p.z-s)*0.57735027;
}

float sdOctahedron2( vec3 p, float s)
{
  p = abs(p);
  float m = p.x+p.y+p.z-s;
  vec3 q;
  if( 3.0*p.x < m ) q = p.xyz;
  else if( 3.0*p.y < m ) q = p.yzx;
  else if( 3.0*p.z < m ) q = p.zxy;
  else return m*0.57735027;
    
  float k = clamp(0.5*(q.z-q.y+s),0.0,s); 
  return length(vec3(q.x,q.y-s+k,q.z-k)); 
}


float sdRhombTriac(vec3 p) {
    float d = sdBox(p, vec3(1.0));
    float c = cos(PI / 5.0);
    float s = sqrt(0.75 - c*c);
    vec3 n = vec3(-0.5, -c, s);

    p = abs(p);
    p -= 2.0 * min(0.0, dot(p, n)) * n;
    
    p.xy = abs(p.xy);
    p -= 2.0 * min(0.0, dot(p, n)) * n;

    p.xy = abs(p.xy);
    p -= 2.0 * min(0.0, dot(p, n)) * n;
    
    d = p.z - 1.0;
    return d;
}

float sdRhombDodeca(vec3 p) {
    p = abs(p);
    p += p.yzx;
    return (max(max(p.x,p.y),p.z)-1.) * sqrt(.5);
}

float opIntersection( float d1, float d2 ) { return max(d1,d2); }

float opSmoothIntersection( float d1, float d2, float k ) {
    float h = clamp( 0.5 - 0.5*(d2-d1)/k, 0.0, 1.0 );
    return mix(d2, d1, h) + k*h*(1.0-h);
}

float sdLens(vec3 p, float s) {
    float offs = 5.75;
    return opSmoothIntersection(
            sdSphere(p - vec3(offs, 0., 0.), s),
            sdSphere(p - vec3(-offs, 0., 0.), s ),
            0.05);
}

float scene(vec3 p) {
    vec3 bpos = p - vec3(4., 0., 0.);
    bpos.xz *= rot2(time * 0.4);
    //bpos.xy *= rot2(time * 0.5);
    
    vec3 tpos = p - vec3(-3., 0., 0.);
    tpos.xz *= rot2(time * 0.5);
    
    vec3 opos = p - vec3(-4., 0., 0.);
    opos.xz *= rot2(time * -0.5);
    opos.yz *= rot2(time * -0.4);

    float box = sdRoundBox(bpos, vec3(1), 0.15);
    float sphere = sdSphere(p - vec3(0., sin(time), 0.), 1.41);
    
    float octa = sdOctahedron2(opos, 2.0) - 0.15;
    
    float rd = sdRhombDodeca(p/2.0 - vec3(0.0, 0.0, 3.0)) * 2.0;

    vec3 lpos = p - vec3(0., 5., 0.);
    //lpos.xz *= rot2(PI*0.5);
    
    vec3 rtpos = p - vec3(4., 0., 0.);
    rtpos.xz *= rot2(time * -0.5);
    rtpos.yz *= rot2(time * -0.3);
    //float rhombtri = sdRhombTriac(rtpos * 0.5) / 0.5;

    float lens = sdLens(lpos, 6.);
    //float tprism = sdTriPrism(tpos, vec2(1.0, 1.0));
    //float torus = sdTorus(tpos, vec2(1., 0.5));
    float objects = min(box, sphere);
    //float objects = min(rhombtri, sphere);
    objects = min(objects, octa);
    objects = min(objects, lens);
    objects = min(objects, rd);
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
    //st.y = 1.0 - st.y;
    return texture2D(tex, st).rgb;

}


vec3 eqrect(sampler2D tex, vec3 rd) {
    vec2 st = vec2(atan(rd.z, rd.x) + PI, acos(rd.y)) / vec2(2.0 * PI, PI);
    return texture2D(tex, st).rgb;
}


void main(void)
{
    float ar = resolution.x / resolution.y;
    vec2 uv = (vertTexCoord.st - vec2(ar*0.5, 0.5)) * -1.;

    const float IOR = 1.5;

    vec3 ro = cam_pos;
    vec3 rd = cameraray(uv, ro, cam_lookat, 0.5);
    vec3 col = vec3(0);
    float d = raymarch(ro, rd, 1.0);

    if(d < MAX_DIST) {
        vec3 p = ro + rd * d;
        vec3 n = getnormal(p);// * 0.5;
        float fresnel = pow(1.0 + dot (rd, n), 3.0);
        vec3 rdIn = refract(rd, n, 1.0/IOR);      // internal ray dir
        vec3 pEnter = p - n * SURF_DIST * 2.0;    // p with offset inside surface
        float dIn = raymarch(pEnter, rdIn, -1.0); // distance marched inside
        vec3 pExit = pEnter + rdIn * dIn;         // exit point
        vec3 nExit = -getnormal(pExit);           // normal at exit point
        vec3 rdOut = refract(rdIn, nExit, IOR);   // dir of ray exiting surface
        if (dot(rdOut, rdOut) == 0.0) {
            //rdOut = reflect(rdIn, nExit) * (1.0 - fresnel); // total internal reflection
            rdOut = reflect(rdIn, nExit);         // total internal reflection
        }
        col = eqrect(tex1, rdOut);
        //col = eqrect(tex1, rdOut) * (1.-fresnel);
        //col += (n * 0.5 + 0.5) * 0.25;
        vec3 ref = reflect(rd, n);
        vec3 refcol = eqrect(tex1, ref);
        //col = vec3(fresnel);
        //col += refcol;
        col = mix(col, refcol, fresnel);

        //col = 1 - col;
        //float diffuse = dot(n, normalize(vec3(1,2,3)))*.5+.5;
        //col *= vec3(diffuse * 1.);
    }
    else {
        //col = sky(tex1, rd);
        col = eqrect(tex1, rd);
        //col = vec3(.2, .3, .7);
    }

    //col = texture2D(tex1, vertTexCoord.st).rgb;
    col = pow(col, vec3(gamma));
    gl_FragColor = vec4(col, 1.0);
}
