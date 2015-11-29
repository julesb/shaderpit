#extension GL_EXT_gpu_shader4 : enable

#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

#define MAX_RAY_STEPS 256
#define SPREAD vec3(150.0, 0.0, 150.0)
/*
   Based on tutorial at:
   http://www.geeks3d.com/20130524/building-worlds-with-distance-functions-in-glsl-raymarching-glslhacker-tutorial-opengl/
*/
vec3 prim_cols[9];

uniform sampler2D texture;
uniform float aspect_ratio;
//uniform float mousex;
//uniform float mousey;
uniform float framecount;
//uniform float swidth;
//uniform float sheight;
uniform vec3 cam_pos;
uniform vec3 cam_lookat;
uniform float cam_fov;
uniform float time;
uniform float blend_coef;
varying vec4 vertColor;
varying vec4 vertTexCoord;
uniform float ray_hit_epsilon;
uniform float palette_offset;
uniform float gamma;
uniform float glow_intensity;

float PI=3.14159265;

const float NOISE_DETAIL =0.01;
const float NOISE_DETAIL2 =0.2;

//const float MB_INNER_SPHERE = 0.72;
//const float MBOX_SCALE = 8.0;
//const float MBULB_SCALE = 128.0;

vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec4 mod289(vec4 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec4 permute(vec4 x) { return mod289(((x*34.0)+1.0)*x); }
vec4 taylorInvSqrt(vec4 r) { return 1.79284291400159 - 0.85373472095314 * r; }

float length2(vec3 p, float n) {
//    (x^n+y^n+z^n)^(1/n)
    return pow((pow(p.x,n) + pow(p.y, n) + pow(p.z, n)), 1.0 / n);
}

float snoise(vec3 v)
  { 
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );
  vec3 x1 = x0 - i1 + C.xxx;
  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
  vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y
  i = mod289(i); 
  vec4 p = permute( permute( permute( 
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));
  float n_ = 0.142857142857; // 1.0/7.0
  vec3  ns = n_ * D.wyz - D.xzx;
  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)
  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)
  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);
  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );
  //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
  //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));
  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;
  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
  }



vec3 to_spherical(vec3 v) {
    v = v.xzy;
    float r = length(v); //sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
    float lon = acos(v.x / sqrt(v.x * v.x + v.y * v.y)) * (v.y < 0.0 ? -1.0 : 1.0);
    float lat = acos(v.z / r);
    return vec3(lon,lat,r);
}

float hash( float n )
{
    return fract(sin(n)*43758.5453);
}


vec3 hash( vec3 n )
{
    return fract(sin(n)*43758.5453);
}

float noise(float p){
    float fl = floor(p);
    float fc = fract(p);
    return mix(hash(fl), hash(fl + 1.0), fc);
}


float noise( vec3 x )
{
    // The noise function returns a value in the range -1.0f -> 1.0f
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
    float n = p.x + p.y*57.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+0.0), hash(n+1.0),f.x),
                   mix( hash(n+57.0), hash(n+58.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+170.0), hash(n+171.0),f.x),f.y),f.z);
}

float noise2d(in vec2 x)
{
    vec2 p = floor(x);
    vec2 f = fract(x);
    f = f*f*(3.0-2.0*f);
    float n = p.x + p.y*157.0;
    return mix(mix(hash(n+0.0), hash(n+1.0),f.x), mix(hash(n+157.0), hash(n+158.0),f.x),f.y);
}

float sd_plane(in vec3 p, in vec3 n, in float o) {
    return dot(p, n) + o; 
}

vec2 obj_floor(in vec3 p) {
    return vec2(p.y+0.0,0.0);
}

vec2 obj_sphere(in vec3 p, float r) {
    float d = length(p) -r;
    return vec2(d,8.0);
}

vec2 obj_torus(in vec3 p) {
    vec2 r = vec2(16.0,8.0);
    vec2 q = vec2(length(p.xz)-r.x,p.y);
    float d = length(q)-r.y;
    return vec2(d,2.0);
}

vec2 obj_round_box(in vec3 p) {
    float d = length(max(abs(p)-vec3(16.0,16.0,16.0),0.0))-4.0;
    return vec2(d,5.0);
}

vec2 obj_box( vec3 p, vec3 b ){
  vec3 d = abs(p) - b;
  return vec2(min(max(d.x,max(d.y,d.z)),0.0)+length(max(d,0.0)), 5.0);
}

vec2 obj_cylinder( vec3 p, vec3 c ) {
  return vec2(length(p.xz-c.xy)-c.z, 8.0);
}

vec2 obj_capsule(vec3 p, vec3 a, vec3 b, float r ) {
    vec3 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return vec2(length( pa - ba*h ) - r, 8.0);
}


vec2 op_union(vec2 a, vec2 b) {
    float d = min(a.x, b.x);
    if (d < b.x)
        return vec2(d,a.y);
    else
        return vec2(d,b.y);
}

vec2 op_sub(vec2 a, vec2 b) {
    float d = max(a.x, -b.x);
    if (d < b.x)
        return vec2(d,a.y);
    else
        return vec2(d,b.y);
}

vec2 op_intersect(vec2 a, vec2 b) {
    float d = max(a.x, b.x);
    if (d <= b.x)
        return vec2(d,b.y);
    else
        return vec2(d,a.y);
}


vec2 op_blend(vec3 p, vec2 a, vec2 b) {
    float s = smoothstep(length(p), 0.0, 1.0);
    float d = mix(a.x, b.x, s);
    if (s < 0.5)
        return vec2(d,a.y);
    else
        return vec2(d,b.y);
}

float smoothmax( float a, float b, float k )
{
    return -log(exp(k*a) + exp(k*b))/-k;
}

float smoothmin( float a, float b, float k )
{
    return -log(exp(-k*a) + exp(-k*b))/k;
}

float smin( float a, float b, float k ) {
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}


vec2 op_sblend(vec3 p, vec2 a, vec2 b) {
    float sm = smin(a.x, b.x, blend_coef);
    //float c = smin(a.y, b.y, blend_coef);
    //if (sm == a.y)
    //    c = a.y
    float c = min(a.y, b.y);
    return vec2(sm, c);
}

vec2 op_sblendx(vec3 p, vec2 a, vec2 b) {
    float sm = smoothmax(a.x, b.x, blend_coef);
    float c = smoothmax(a.y, b.y, blend_coef);
    return vec2(sm, c);
}


vec3 op_rep(vec3 p, vec3 spread) {
    return mod(p+spread*0.5, spread) - spread*0.5;
}

vec2 obj_sine(vec3 p) {
    p = floor(p);
    return vec2((sin(p.x * 0.2059)
               + sin(p.y * 0.231)
               + sin(p.z * 0.226)) * 0.333, 4.0);
}

vec2 op_displace(vec3 p, vec2 obj) {
    float m = framecount * 0.1;
    vec3 v = cos(p*0.1+m);
    float d = v.x*v.y*v.z * 2.0;
    return vec2(obj.x+d, obj.y);
}

vec3 deform( in vec3 p, in float time, out float sca )
{
    float s = 0.034*sqrt(dot(p*p,p*p));
    //float s = 1.0;

    p = p/s;

    //p.xyz += 16.0*sin(0.5*vec3(1.0,1.1,1.3)*time+vec3(0.0,2.0,4.0));
    
    sca = s;
    
	return p;
}

vec3 rotate_x(in vec3 p, float an) {
    float c = cos(an);
    float s = sin(an);
    return vec3(p.x, c * p.y - s * p.z, s * p.y + c * p.z);
}
vec3 rotate_y(in vec3 p, float an) {
    float c = cos(an);
    float s = sin(an);
    return vec3(c * p.x + s * p.z, p.y, -s * p.x + c * p.z);
}
/*
vec3 rotate_y(vec3 p, float a) {
	float c,s;vec3 q=p;
	c = cos(a); s = sin(a);
	p.x = c * q.x + s * q.z;
	p.z = -s * q.x + c * q.z;
    return p;
}
*/

vec3 rotate_z(in vec3 p, float an) {
    float c = cos(an);
    float s = sin(an);
    return vec3(c * p.x - s * p.y, s * p.x + c * p.y, p.z);
}

vec3 get_rep_id(vec3 p, vec3 spread) {
    return floor((p+spread*0.5) / spread);
}

vec2 obj_ufo(vec3 p) {
    return op_sblend(p,
        op_union(
            op_displace(p, obj_sphere(p, 1.25)),
            op_sub(op_displace(p, obj_round_box(p)),
                     op_displace(p, obj_torus(p)))),
        op_sblend(p,
            op_displace(p, obj_capsule(p, vec3(30.5, 0.0, 0.0), vec3(-30.5, 0.0, 0.0), 5.95 )),
            op_displace(p, obj_capsule(p, vec3(0.0, 0.0, -30.5), vec3(0.0, 0.0, 30.5), 5.95 )) )
        //obj_capsule(p, vec3(0.0, 1.5, 0.0), vec3(0.0, -1.5, 0.0), 0.5 )
        //op_displace(p, obj_capsule(p, vec3(0.0, 1.5, 0.0), vec3(0.0, -1.5, 0.0), 0.5 ))
        
        );
}

vec2 obj_invertedufo(vec3 p) {
    float s = 1.0;
    vec3 p2 = (deform(p,float(framecount)*0.0333, s));
    vec2 g = obj_ufo(p2) * vec2(s, 1.0);
    return g;
}



/*
vec2 obj_ufo(vec3 p) {
    return op_union(
        op_union(
            op_union(op_displace(p, obj_round_box(p)),
                     op_displace(p, obj_torus(p))),
            op_displace(p, obj_sphere(p, 1.5)) ),
        op_union(
            op_displace(p, obj_capsule(p, vec3(2.5, 0.0, 0.0), vec3(-2.5, 0.0, 0.0), 0.75 )),
            op_displace(p, obj_capsule(p, vec3(0.0, 0.0, -2.5), vec3(0.0, 0.0, 2.5), 0.75 )) )
        //obj_capsule(p, vec3(0.0, 1.5, 0.0), vec3(0.0, -1.5, 0.0), 0.5 )
        //op_displace(p, obj_capsule(p, vec3(0.0, 1.5, 0.0), vec3(0.0, -1.5, 0.0), 0.5 ))
        
        );
}
*/
vec2 op_noise(vec3 p, vec2 obj) {

    return vec2(obj.x + 0.5*snoise(p*NOISE_DETAIL), obj.y);
}

vec2 obj_noise(vec3 p) {
    return vec2(length(p), 5.1);
}

vec2 obj_cross(vec3 p, float r) {
    float inf = 1.0 / 0.0;
    //vec2 b1 = obj_box(p.xyz, vec3(inf,r,r));
    //vec2 b2 = obj_box(p.yzx, vec3(r,inf,r));
    //vec2 b3 = obj_box(p.zxy, vec3(r,r,inf));
    vec2 b1 = obj_cylinder(p.xyz, vec3(0.0,0.0,r));
    vec2 b2 = obj_cylinder(p.yzx, vec3(0.0,0.0,r));
    vec2 b3 = obj_cylinder(p.zxy, vec3(0.0,0.0,r));
    return min(b1,min(b2,b3));
    //return op_sblend(p, b1, op_sblend(p, b2, b3));
}

vec2 obj_repcross(vec3 p, vec3 c) {
    vec3 q = mod(p,c)-0.5*c;
    return obj_cross(q, 2.0);
}

vec2 obj_grid(vec3 p) {
    return obj_repcross(p, vec3(300.0,300.0,300.0));
}
vec2 obj_invertedgrid(vec3 p) {
    float s = 1.0;
    //vec3 p2 = (deform(p,float(framecount)*0.01333, s));
    vec3 p2 = (deform(p,float(framecount)*0.01333, s));
    vec2 g = obj_grid(p2) * vec2(s, 1.0);
    return g;
}
vec2 obj_invertedgrid_rep(vec3 p, vec3 c) {
    vec3 q = mod(p,c)-0.5*c;
    return obj_invertedgrid(q) ;
}

float sd_tetra(in vec3 p) {
    float s = 2.0;
  	vec3 a1 = normalize(vec3(1,1,1));
	vec3 a2 = normalize(vec3(-1,-1,1));
	vec3 a3 = normalize(vec3(1,-1,-1));
	vec3 a4 = normalize(vec3(-1,1,-1));
    float p1 = sd_plane(p, a1, s);
    float p2 = sd_plane(p, a2, s);
    float p3 = sd_plane(p, a3, s);
    float p4 = sd_plane(p, a4, s);
    float d = min(min(min(p1, p2), p3), p4);
    return -d;
}

vec2 obj_tetrahedron(in vec3 p, in float s) {
    return vec2(sd_tetra(p/s) * s, 7.0);
}

vec3 prim_color(in vec3 p, float i) {
    i = mod(i, 10.0);
    prim_cols[0] = vec3(0.15,0.15,0.15);
    prim_cols[1] = vec3(1.0,0.0,0.0);
    prim_cols[2] = vec3(1.0,0.25,0.0);
    prim_cols[3] = vec3(1.0,1.0,0.0);
    prim_cols[4] = vec3(0.0,1.0,0.0);
    prim_cols[5] = vec3(0.0,0.0,1.0);
    prim_cols[6] = vec3(1.0,0.0,1.0);
    prim_cols[7] = vec3(0.5,0.0,1.0);
    prim_cols[8] = vec3(1.0,1.0,1.0);
    i = floor(i);

    if (i < 0.0)
        return vec3(0.5); //floor_color(p);
    else if (i <= 8.0) {
        return prim_cols[int(i)];
    }
    else {
        return vec3(0.01,0.01,0.01);
    }
}


void rY(inout vec3 p, float a) {
	float c,s;vec3 q=p;
	c = cos(a); s = sin(a);
	p.x = c * q.x + s * q.z;
	p.z = -s * q.x + c * q.z;
}

vec2 obj_boxsphere_perms(in vec3 p) {
    float yoff = 30.0;
    return
        op_union(
            op_union(
                obj_floor(p),
                op_union(
                    op_union(
                        op_sub(
                             obj_round_box(p-vec3(150.0, yoff, 0.0)),
                             obj_sphere(p-vec3(150.0, yoff, 0.0), 32.0)
                        ),
                        op_sub(
                             obj_sphere(p-vec3(50.0, yoff, 0.0), 32.0),
                             obj_round_box(p-vec3(50.0, yoff, 0.0))
                        )
                    ),
                    op_union(
                        obj_sphere(p-vec3(-50.0, yoff, 0.0), 32.0),
                        obj_round_box(p-vec3(-50.0, yoff, 0.0))
                    )
                )
            ),
            op_intersect(
                obj_sphere(p-vec3(-150.0, yoff, 0.0), 32.0),
                obj_round_box(p-vec3(-150.0, yoff, 0.0))
            )
        )
    ;
}

vec2 obj_tetra_caps(in vec3 p) {
    return
          //op_sblend(p,
          op_union(
            obj_tetrahedron(p-vec3(0.0,20.0,0.0), 4.0),
            obj_capsule(p, vec3(20.5, 20.0, 0.0), vec3(-20.5, 20.0, 0.0), 10.0 ));
}
vec2 obj_boxsubsphere(in vec3 p) {
    return op_sub(
         obj_round_box(p),
         obj_sphere(p, 24.0)
         );
}

vec2 random_prim(vec3 p, float h) {
    //float phase = noise2d(id.xz) * 2.0 * PI; 
    //float dir = (hash(id.x*id.z) < 0.0)? -1.0: 1.0;
    //float dir = (noise(id) < 0.5)? -1.0:1.0;
    //float speed = (noise2d(id.xz*NOISE_DETAIL2) - 0.0) * 1.0;
    float speed = 1.0;
    h *= 1.1;
    if (h < 0.1) {
        p = rotate_y(p, time*0.001*speed*0.73);
        return obj_box(p, vec3(20.0,20.0,20.0));
    }

    if (h < 0.2) {
        return obj_sphere(p, 20.0);
    }
    else if (h < 0.3) {
        p = rotate_y(p, time*0.001*speed);
        return obj_capsule(p, vec3(-20.0, 0.0, 0.0), vec3(20.0, 0.0, 0.0), 16.0);
    }
    else if (h < 0.4) {
        p = rotate_y(p, time*0.001*speed*1.3);
        p = rotate_z(p, time*0.001*speed*1.431);
        p = rotate_x(p, time*0.001*speed*1.247);
        return obj_torus(p-vec3(0.0,8.0,0.0));
    }
    else if (h < 0.5) {
        p = rotate_y(p, time*0.001*speed*-0.79);
        return obj_tetrahedron(p, 4.0);
        //return obj_round_box(p);
    }
    else if (h < 0.6) {
        return obj_sphere(p, 2.0);
        //return obj_tetrahedron(p, 8.0);
    }
    else if (h < 0.7) {
        p = rotate_y(p, time*0.001*speed);
        //p = rotate_x(p, time*speed);
        return obj_tetra_caps(p);
    }
    else if (h < 0.8) {
        p = rotate_x(p, time*0.001*speed);
        return obj_capsule(p, vec3(0.0, -20.0, 0.0), vec3(0.0, 20.0, 0.0), 16.0);
    }
    else if (h < 0.9) {
        p = rotate_y(p, time*0.001*speed);
        return obj_capsule(p, vec3(32.0, 16.0, 0.0), vec3(-32.0, 16.0, 0.0), 8.0);
    }
    else if (h < 1.0) {
        p = rotate_y(p, time*0.001*speed*0.4);
        p = rotate_z(p, time*0.001*speed*0.431);
        p = rotate_x(p, time*0.001*speed*0.447);
        return obj_box(p, vec3(16.0,16.0,16.0));
    }
    else {
        return obj_sphere(p, 8.0);
        //return vec2(99999.0, 5.0);
    }
}
/*
vec2 cube_field(vec3 p, float h, vec3 id, float soff) {
    float cs = 20.0;
    float n = (1.0+floor(noise2d(id.xz*NOISE_DETAIL2) * 2.0)) * cs;
    return op_sblend(p, obj_box(p - vec3(0.0, cs+soff, 0.0), vec3(cs,n,cs)),
                        random_prim(p-vec3(0.0,n+cs+soff*2.0,0.0), h, id));
}
*/

vec2 hash_placement(vec3 p, float n) {
    vec3 pos;
    float d = 9999.0; 
    float i=0.0;
    float sec = floor(time*0.01);
    for (i=0.0; i < n; i++) {
        pos = vec3(hash(i*sec), 0.0, hash(i*sec+1.3323));
        pos -= 0.5;
        pos *= vec3(100.0, 0.0, 100.0);
        pos += vec3(0.0,24.0,0.0);
        //d = min(d, obj_sphere(p-pos, 24.0).x);
        //d = smin(d, obj_sphere(p-pos, 24.0).x, blend_coef);
        d = smin(d, obj_sphere(p-pos, 24.0).x, blend_coef);
    }
    return vec2(d, 4.0);
}

vec2 noise_placement(vec3 p, float n) {
    vec3 pos;
    float d = 9999.0; 
    float i=0.0;
    float sec = time*0.000125;
    vec3 c;
    for (i=0.0; i < n; i++) {
        pos = vec3(noise(i+sec), 0.5, noise(i+sec+1.3323));
        pos -= 0.5;
        pos *= vec3(300.0, 0.0, 300.0);
        pos += vec3(0.0,40.0,0.0);
        //d = min(d, obj_sphere(p-pos, 24.0).x);
        //d = smin(d, random_prim(p-pos, i*0.1).x, blend_coef);
        d = smin(d, obj_sphere(p-pos, 40.0).x, blend_coef);
        c = prim_color(p-pos, i);
    }
    return vec2(d, 5.0);
}

vec2 rand_placement(vec3 p, float n) {
    vec3 pos;
    float d = 9999.0; 
    float i=0.0;
    vec3 h = hash(floor(p) + floor(time*0.001));
    pos = h * 30.0;
    pos.y += 4.0;

    if (h.x+h.y+h.z < 0.5) {
        return obj_sphere(p-pos, 4.0);
    }
    else {
        return obj_floor(p);
    }

}

vec2 distance_to_obj(in vec3 p) {

    //p = rotate_y(p, framecount * 0.01);
    //vec3 id = get_rep_id(p, SPREAD);
    //vec3 prep = op_rep(p, SPREAD);
    //float t = framecount / 5.0;// * 2.0 * PI;
    //float h = hash(id.x*id.y) * 0.5 + 0.5;
    //float sig = (int(id.x*id.y) % 2  == 0)? -1.0:1.0;
    //float phase = noise2d(id.xz * NOISE_DETAIL2) * 2.0 * PI + (sig*PI*1.0);
    //float phase = hash(id.x*id.z) * 2.0 * PI;
    //float soff = ((sin(t+phase)* 0.5 + 0.5)) * 64.0 - 32.0;
    //float obidx =  hash(id.x*id.z); // * 0.5 + 0.5;
    //float obidx =  noise2d(id.xz * NOISE_DETAIL2); // * 0.5 + 0.5;
    vec2 scene1 = op_union(
                  //op_sblend(p, 
                               obj_floor(p),
                               //obj_sphere(p, 4.0));
                               noise_placement(p, 5.0));
                  //             cube_field(prep, obidx, id, soff))

                //* vec2(0.5, 1.0);
/*
    vec2 scene1 = 
        op_sblend(p, 
        //op_union( 
            obj_floor(p),
            random_prim(prep-vec3(0.0,soff,0.0), obidx, id)
        )
        * vec2(0.5, 1.0);
*/
        return scene1;


}




vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( palette_offset +  6.28318*(c*t+d) );
}

vec3 ansi_gradient(float t) {
	return mod(floor(t * vec3(8.0, 4.0, 2.0)), 2.0);
}

float sqr(float n) {return n*n;}

vec3 rainbow_gradient(float t) {
	vec3 c = 1.0 - pow(abs(vec3(t) - vec3(0.65, 0.5, 0.2)) * vec3(3.0, 3.0, 5.0), vec3(1.5, 1.3, 1.7));
	c.r = max((0.15 - sqr(abs(t - 0.04) * 5.0)), c.r);
	c.g = (t < 0.5) ? smoothstep(0.04, 0.45, t) : c.g;
	return clamp(c, 0.0, 1.0);
}
vec3 heatmap_gradient(float t) {
	return clamp((pow(t, 1.5) * 0.8 + 0.2) * vec3(smoothstep(0.0, 0.35, t) + t * 0.5, smoothstep(0.5, 1.0, t), max(1.0 - t * 1.7, t * 7.0 - 6.0)), 0.0, 1.0);
}
vec3 neon_gradient(float t) {
	return clamp(vec3(t * 1.3 + 0.1, sqr(abs(0.43 - t) * 1.7), (1.0 - t) * 1.7), 0.0, 1.0);
}

vec3 stripe_gradient(float t) {
	return vec3(mod(floor(t * 32.0), 2.0) * 0.2 + 0.8);
}

vec3 grey_gradient(float t) {
	return vec3(clamp(0.0,1.0, t));
}

vec3 fire_gradient(float t) {
	return max(pow(vec3(min(t * 1.02, 1.0)), vec3(1.7, 25.0, 100.0)), 
			   vec3(0.06 * pow(max(1.0 - abs(t - 0.35), 0.0), 5.0)));
}

vec3 floor_color(in vec3 p) {
    float m = 0.5;
    vec3 c = vec3(0.0); //get_integer_circles_color(p.xz, vec3(1.0,1.0,1.0));
    if (fract(p.x*m)>m) {
        if (fract(p.z*m)>m)
            return vec3(0,0.1,m) + c;
        else
            return vec3(0,0,0) + c;
    }
    else {
        if (fract(p.z*m)>m)
            return vec3(0,0,0) + c;
        else
            return vec3(0,0.3,0) + c;
    }
}


// Rainbow (more yellow, narrower green, deeper red)
vec3 rainbow2_gradient(float t) {
    return pal(t, vec3(0.55,0.4,0.3),vec3(0.50,0.51,0.35)+0.1,vec3(0.8,0.75,0.8),vec3(0.075,0.33,0.67)+0.21);
}


vec3 lambert(vec3 p, vec3 n, vec3 l) {
    return vec3(max( dot(normalize(l-p), n), 0.0));
   // return clamp(vec3(dot(normalize(l-p), n)), 0.0, 1.0);
}


// http://www.iquilezles.org/www/articles/rmshadows/rmshadows.htm

float iqshadow( in vec3 ro, in vec3 rd, float mint, float maxt ) {
    for( float t=mint; t < maxt; ) {
        float h = distance_to_obj(ro + rd*t).x;
        if( h<0.001 )
            return 0.0;
        t += h;
    }
    return 1.0;
}

float iqsoftshadow( in vec3 ro, in vec3 rd, float mint, float maxt, float k ) {
    float res = 1.0;
    for( float t=mint; t < maxt; ) {
        float h = distance_to_obj(ro + rd*t).x;
        if( h < 0.001)
            return 0.0;
        res = min( res, k*h/t );
        t += h;
    }
    return res;
}

vec3 iqfog( in vec3  rgb,      // original color of the pixel
            in float camdist, // camera to point distance
            in vec3  rayDir,   // camera to point vector
            in vec3  sunDir ) {// sun light direction
    float fogAmount = 1.0 - exp( -camdist*0.00012512565125 );
    float sunAmount = max( dot( rayDir, sunDir ), 0.0 );
    vec3  fogColor  = mix( vec3(0.5,0.6,0.7), // bluish
                           vec3(1.0,0.9,0.7), // yellowish
                           pow(sunAmount,256.0) );
    return mix( rgb, fogColor, fogAmount );
    //return mix( fogColor, vec3(0.0,0.0,0.0), fogAmount );
}


vec3 sun( in vec3  rgb,      // original color of the pixel
          in vec3  rayDir,   // camera to point vector
          in vec3  sunDir ) {// sun light direction
    //sunDir.y = abs(sunDir.y);
    float sunAmount = max( dot( rayDir, sunDir ), 0.0 );
    vec3  sunColor  = vec3(1.0,0.9,0.8) * pow(sunAmount,1024.0);
    return rgb + sunColor;
}

vec3 saturate(vec3 a) { return clamp(a, 0.0, 1.0); }
vec2 saturate(vec2 a) { return clamp(a, 0.0, 1.0); }
float saturate(float a) { return clamp(a, 0.0, 1.0); }

float getao(vec3 p, vec3 n) {
    float ao = 1.0;
    float amb = 0.2;
    ao *= saturate(distance_to_obj(p + n * 0.0125).x*80.0);
    ao *= saturate(distance_to_obj(p + n * 0.025).x*40.0);
    ao *= saturate(distance_to_obj(p + n * 0.05).x*20.0);
    ao *= saturate(distance_to_obj(p + n * 0.1).x*10.0);
    ao *= saturate(distance_to_obj(p + n * 0.2).x*5.0);
    ao *= saturate(distance_to_obj(p + n * 0.4).x*2.5);
    return amb + (1.0-amb)*clamp(ao*8.0, 0.0, 1.0);
}

float calcAO( in vec3 pos, in vec3 nor ) {
	float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<4; i++ ) {
        float hr = 0.01 + float(i)/0.0525;
        //float hr = 0.01 + 0.12*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = distance_to_obj( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.01225;
    }
    return clamp( 1.5 - occ*4.0, 0.0, 1.0 );
}


vec3 calcNormal( in vec3 pos ) {
	vec3 eps = vec3( 0.01, 0.0, 0.0 );
	vec3 nor = vec3(
	    distance_to_obj(pos+eps.xyy).x - distance_to_obj(pos-eps.xyy).x,
	    distance_to_obj(pos+eps.yxy).x - distance_to_obj(pos-eps.yxy).x,
	    distance_to_obj(pos+eps.yyx).x - distance_to_obj(pos-eps.yyx).x );
	return normalize(nor);
}

vec3 getNormal( in vec3 p ) {
    return normalize(cross( dFdy(p), dFdx(p) ));
}


void main(void) {
    vec2 q = vertTexCoord.st;
    vec2 vPos = -vec2(aspect_ratio, 1.0) + 2.0 * q;
    //vec2 vPos = -1.0 + 2.0 * q;

    // Camera up vector.
    vec3 vuv=vec3(0,-1,0); 

    // Camera lookat.
    vec3 vrp = cam_lookat;

    // Camera pos
    vec3 prp = cam_pos;
    // Camera setup.
    vec3 vpn=normalize(vrp-prp);
    //vec3 u = normalize(cam_pos);
    vec3 u=normalize(cross(vuv,vpn));
    vec3 v=cross(vpn,u);
    vec3 vcv=(prp+vpn);
    //vec3 scrCoord=vcv+vPos.x*u*swidth/sheight+vPos.y*v;
    //vec3 scrCoord=vcv+vPos.x*u*resolution.x/resolution.y+vPos.y*v;
    float fov = cam_fov; //PI/3.0;
    vec3 scrCoord=vcv + vPos.x*u*fov
                      + vPos.y*v*fov;
    vec3 scp=normalize(scrCoord-prp);

    float lightspeed = 0.0125;
    float lightrad = 2500.0;
/*    
    vec3 lightpos = cam_pos+vec3(cos(PI/4.0 + framecount*lightspeed)*lightrad,
                         3000.0,
                         sin(PI/4.0 + framecount*lightspeed)*lightrad);
*/    
    //vec3 lightpos = vec3(cam_pos.x,500.0,cam_pos.z);
    //vec3 lightpos = normalize(cam_pos*vec3(-1.0, 1.0, 1.0)) * 500.0;
    vec3 lightpos =  vec3(0.0,100.0,0.0);
    
    //vec3 lightpos = cam_pos + normalize(vpn)*1.0 + u*0.01;

    // Raymarching.
    const vec3 e=vec3(0.02,0,0);
    const float maxd=1000.0; //Max depth
    vec2 d=vec2(0.01,0.0);
    vec3 c,p,N;

    float f=0.01; // near plane?
    
    float nsteps = 0.0;

//    vec2 cam_dist = distance_to_obj(cam_pos);
//    if (cam_dist.x < 0.0) {
//        gl_FragColor=vec4(0.25*prim_color(cam_pos, int(cam_dist.y)),1.0);
//    }
//    else {
    for(int i=0;i<MAX_RAY_STEPS;i++) {
        if ((d.x < ray_hit_epsilon) || (f > maxd)) {
            //p += (vec3(hash(p.x), hash(p.y), hash(p.z)) -= 0.5) * 0.001;
            //d = distance_to_obj(p);
            break;
        }
        f+=d.x;
        p=prp+scp*f;
        d = distance_to_obj(p);
        nsteps = nsteps + 1.0;
    }


    float AO;
    vec3 n = vec3(d.x-distance_to_obj(p-e.xyy).x,
                  d.x-distance_to_obj(p-e.yxy).x,
                  d.x-distance_to_obj(p-e.yyx).x);
    N = normalize(n);

    //N = getNormal(p);
    //N = calcNormal(p);
    
    //AO = getao(p, N) * 0.5;
    AO = calcAO(p, N);
    //AO = AO*AO*AO;

    //AO = 1.0;
    //AO = getao(p, N) * 1.0;
    vec3 glowcol_miss = rainbow2_gradient(AO*1.0);
    vec3 glowcol = (rainbow2_gradient(AO*1.0));// + vec3(1.0, 1.0, 1.0)) * 0.5 ;
    //c = prim_color(p, d.y);
    //c = prim_color(p, mod(id.x*id.z, 12.0));
    
    c = prim_color(p, d.y);
    //if (d.y  > 0.0)
    //c *= 1.0 - rainbow2_gradient( noise2d(id.xz*NOISE_DETAIL2));
    //c = rainbow2_gradient(AO*1.0);

    float cam_dist = length(cam_pos - p);

    vec3 spher = to_spherical(reflect(normalize(cam_pos-p), N ));
    vec2 uv = spher.xy / vec2(2.0*PI, PI);
    uv.y = 1.0 - uv.y;
    vec3 texcol = texture2D(texture, uv).rgb;

/*
    if (d.y == 0.0) {
        texcol = vec3(0.5,0.5,0.5);
        c = vec3(0.5,0.5,0.5);
    }
*/
    //texcol = sun(texcol, normalize(cam_pos-p), normalize(p-lightpos));
    //texcol = iqfog(texcol, cam_dist, normalize(p-cam_pos), normalize(p-lightpos));
    vec3 stepbri = vec3(nsteps/float(MAX_RAY_STEPS));
    vec3 glow = stepbri * c * glow_intensity;
    //vec3 glow_miss = vec3(nsteps/256.0) * glowcol_miss * glow_intensity;
    vec3 glow_miss = stepbri * c * glow_intensity;

    
    if (f < maxd) {
 
        vec3 ambient = vec3(0.07, 0.08, 0.09);
        float amb_shad =0.2;
        float amb_lamb = 0.0;

        vec3 lightdir = normalize(lightpos - p);

        //vec3 cam_dist_sc = vec3(cam_dist/ 256.0);
        float b = dot(N, lightdir);
        vec3 phong;
        if (b < 0.0) {
            phong = vec3(0.0);
        }
        else {
            phong =  vec3((b + pow(b,64.0))); // * (1.0-f*0.005));
        }
        vec3 lamb = amb_lamb + (1.0 - amb_lamb) * lambert(p, N, lightpos);
        //vec3 lamb = lambert(p, N, lightpos);
        float shad = amb_shad + (1.0 - amb_shad) * iqsoftshadow(p, lightdir, 0.1, 800.0, 32.0);

        vec3 fc = 
                (
                mix(texcol, c, 0.25) * 0.333
                //c * 0.333
                + lamb * 0.333
                + phong * 0.333 
                )
                * vec3(shad)
                * AO
                //* stepbri
                +ambient
                ;
        fc += glow;
        fc = iqfog(fc, cam_dist, normalize(cam_pos-p), normalize(p-lightpos));
        fc = pow(fc, vec3(gamma));
        gl_FragColor = vec4(fc, 1.0);
    }
    else {
        vec3 spher2 = to_spherical(normalize(cam_pos-p));
        vec2 uv2 = (spher2.xy / vec2(2.0*PI, PI));
        uv2.y = 1.0 - uv2.y;
        vec3 texcol2 = texture2D(texture, uv2.xy).rgb;
        vec3 bgcol = texcol2; //rainbow2_gradient(1.0);
        //vec3 bgcol = vec3(0.0,0.0,0.0);
        //vec3 bgcol = vec3(0.5, 0.6,0.7); //rainbow2_gradient(1.0);
        //vec3 circ = get_integer_circles_color((-0.5+fract(uv2.xy))*16.0, vec3(1.0));
        //vec3 circ = get_grid_pixel_color((-0.5+fract(uv2.xy))*4.0);
        //bgcol += circ;
        //bgcol = sun(bgcol, normalize(p-cam_pos), normalize(p-lightpos));
        bgcol += glow_miss;
        bgcol = iqfog(bgcol, cam_dist, normalize(cam_pos-p), normalize(p-lightpos));
        bgcol = pow(bgcol, vec3(gamma));
        
        //bgcol = vec3(noise2d(scp.xy*100.0));
        // vec3 glow = vec3(nsteps/256.0) *  vec3(0.8,0.8,1.0) * 1.0;
        gl_FragColor=vec4(bgcol.rgb,1.0); //background color
    }

}
