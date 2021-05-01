//#extension GL_EXT_gpu_shader4 : enable

#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

#define MAX_RAY_STEPS 256
vec3 prim_cols[9];

uniform sampler2D texture;
uniform float aspect_ratio;
uniform float framecount;
uniform float swidth;
uniform float sheight;
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
uniform float diff_spec;

float PI=3.14159265;
vec3 cam_light_pos = vec3(0.0);

vec3 to_spherical(vec3 v) {
    v = v.xzy;
    float r = length(v); //sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
    float lon = acos(v.x / sqrt(v.x * v.x + v.y * v.y)) * (v.y < 0.0 ? -1.0 : 1.0);
    float lat = acos(v.z / r);
    return vec3(lon,lat,r);
}


float sd_plane(in vec3 p, in vec3 n, in float o) {
    return dot(p, n) + o; 
}

vec2 obj_floor(in vec3 p) {
    return vec2(p.y+0.0,4.0);
}

vec2 obj_sphere(in vec3 p, float r) {
    float d = length(p) -r;
    return vec2(d,8.0);
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

float smin( float a, float b, float k ) {
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}


vec2 op_sblend(vec3 p, vec2 a, vec2 b) {
    float sm = smin(a.x, b.x, blend_coef);
    float c = smin(a.y, b.y, blend_coef);
    return vec2(sm, c);
}

float smoothmax( float a, float b, float k )
{
    return -log(exp(k*a) + exp(k*b))/-k;
}

float smoothmin( float a, float b, float k )
{
    return -log(exp(-k*a) + exp(-k*b))/k;
}

vec3 op_rep(vec3 p, vec3 spread) {
    return mod(p+spread*0.5, spread) - spread*0.5;
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
vec3 rotate_z(in vec3 p, float an) {
    float c = cos(an);
    float s = sin(an);
    return vec3(c * p.x - s * p.y, s * p.x + c * p.y, p.z);
}

vec3 get_rep_id(vec3 p, vec3 spread) {
    return floor((p+spread*0.5) / spread);
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

vec3 prim_color(in vec3 p, float i) {
    prim_cols[0] = vec3(0.5,0.5,0.5);
    prim_cols[1] = vec3(1.0,0.0,0.0);
    prim_cols[2] = vec3(1.0,0.25,0.0);
    prim_cols[3] = vec3(1.0,1.0,0.0);
    prim_cols[4] = vec3(0.0,1.0,0.0);
    prim_cols[5] = vec3(0.0,0.0,1.0);
    prim_cols[6] = vec3(1.0,0.0,1.0);
    prim_cols[7] = vec3(0.5,0.0,1.0);
    prim_cols[8] = vec3(1.0,1.0,1.0);
    i = floor(i);

    if (i == 0.0)
        return floor_color(p);
    else if (i <= 8.0) {
        return prim_cols[int(i)];
    }
    else {
        return vec3(0.5,0.5,0.5);
    }

}



// -------------------------------------------------------------------------
// https://www.shadertoy.com/view/4sjSW1

#define RSCALE 2.8
#define MINRAD2 .25
float minRad2 = clamp(MINRAD2, 1.0e-9, 1.0);
vec4 scale = vec4(RSCALE, RSCALE, RSCALE, abs(RSCALE)) / minRad2;
float absScalem1 = abs(RSCALE - 1.0);
float AbsScaleRaisedTo1mIters = pow(abs(RSCALE), float(1-11));
vec3 surfaceColour1 = vec3(.75, 1.0, 0.75);
vec3 surfaceColour2 = vec3(.4, .4, 0.5);
//vec3 surfaceColour3 = vec3(.5, 0.3, 0.00);
vec3 surfaceColour3 = vec3(0.1, 0.1, 0.1);


float remnantx(vec3 pos) 
{
	//return (length(pos)-4.0);

	vec4 p = vec4(pos,1);
	vec4 p0 = p;  // p.w is the distance estimate

	for (int i = 0; i < 9; i++)
	{
		p.xyz = clamp(p.xyz, -1.0, 1.0) * 2.0 - p.xyz;

		// sphere folding: if (r2 < minRad2) p /= minRad2; else if (r2 < 1.0) p /= r2;
		float r2 = dot(p.xyz, p.xyz);
		p *= clamp(max(minRad2/r2, minRad2), 0.0, 1.0);

		// scale, translate
		p = p*scale + p0;
	}
	return ((length(p.xyz) - absScalem1) / p.w - AbsScaleRaisedTo1mIters);
}

vec3 remnantx_color(vec3 pos, float sphereR) 
{
	//scale.z = 0.0;
	vec3 p = pos;
	vec3 p0 = p;
	float trap = 1.0;
    //float rtime = 0.0; //framecount * 0.05;
    float rtime = time * 1.0;
    //float rtime = framecount * 0.05;
	for (int i = 0; i < 6; i++)
	{
        
		p.xyz = clamp(p.xyz, -1.0, 1.0) * 2.0 - p.xyz;
		float r2 = dot(p.xyz, p.xyz);
		p *= clamp(max(minRad2/r2, minRad2), 0.0, 1.0);

		p = p*scale.xyz + p0.xyz;
		trap = min(trap, r2);
	}
	// |c.x|: log final distance (fractional iteration count)
	// |c.y|: spherical orbit trap at (0,0,0)
	vec2 c = clamp(vec2( 0.3333*log(dot(p,p))-1.0, sqrt(trap) ), 0.0, 1.0);

    //float t = mod(length(pos) - rtime*5., 6.0);
    float t = mod((0.5+sin(length(pos) - rtime)*0.5) *5., 6.0) ;
    surfaceColour1 = mix(rainbow2_gradient(palette_offset), surfaceColour1,  pow(smoothstep(0.0, .3, t) * smoothstep(0.6, .3, t), 5.0));
    //surfaceColour1 = mix( surfaceColour1, vec3(.1, 0.5, 1.0), pow(smoothstep(0.0, .3, t) * smoothstep(0.6, .3, t), 10.0));
	return mix(mix(surfaceColour1, surfaceColour2, c.y), surfaceColour3, c.x);
	//return mix(mix(surfaceColour1, surfaceColour3, c.y),rainbow2_gradient(t)  , c.x);
	//return mix(mix(surfaceColour3, rainbow2_gradient(palette_offset), c.y), surfaceColour1, c.x);
}

#define DSCALE 2.8

vec2 distance_to_obj(in vec3 p) {
             //obj_sphere(p + camera_pos, 1.0)
    return
            op_sblend(p,
             obj_floor(p), 
             vec2(remnantx(p/DSCALE)*DSCALE, 1.0));
//    return op_union(
//            obj_sphere(p - cam_light_pos + vec3(0.0, 0.05, 0.0), 0.005),
//            op_sblend(p,
//             obj_floor(p), 
//             vec2(remnantx(p/DSCALE)*DSCALE, 1.0)));
}
// -------------------------------------------------------------------------
vec3 LightSource(vec3 spotLight, vec3 dir, float dis)
{
    float g = 0.0;
    if (length(spotLight) < dis)
    {
		g = pow(max(dot(normalize(spotLight), dir), 0.0), 500.0);
    }
   
    return vec3(.6) * g;
}

vec3 lambert(vec3 p, vec3 n, vec3 l) {
    return vec3(max( dot(normalize(l-p), n), 0.0));
   // return clamp(vec3(dot(normalize(l-p), n)), 0.0, 1.0);
}

vec3 specular(in vec3 N, in vec3 L, in vec3 V) {
   float shininess = 32.0;
   float specularterm = 0.0;

   // calculate specular reflection only if
   // the surface is oriented to the light source
   if(dot(N, L) > 0.0)
   {
      // half vector
      vec3 H = normalize(L + V);
      specularterm = pow(dot(N, H), shininess);  //
   }
    return vec3(1.0,0.9,0.8) * vec3(1.0,1.0,1.0) * specularterm;
   //return u_matSpecularReflectance * u_lightSpecularIntensity * specularTerm;
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
    float fogAmount = 1.0 - exp( -camdist*0.0002512565125 );
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

/*
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
*/

float calcAO( in vec3 pos, in vec3 nor ) {
	float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<10; i++ ) {
        float hr = 0.01 + float(i)/0.0475;
        //float hr = 0.01 + float(i)/0.0525;
        //float hr = 0.01 + 0.12*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = distance_to_obj( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.0125;
    }
    return clamp( 1.1 - occ*4.0, 0.0, 1.0 );
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

float hash( float n )
{
    return fract(sin(n)*43758.5453);
}

float D3DX_SRGB_to_FLOAT(float val)
{
    if( val < 0.04045 )
        val /= 12.92;
    else
        val = pow((val + 0.055)/1.055,2.4);
    return val;
}

vec3 D3DX_SRGB_to_FLOAT(vec3 v) {
    return vec3(D3DX_SRGB_to_FLOAT(v.x),
            D3DX_SRGB_to_FLOAT(v.y),
            D3DX_SRGB_to_FLOAT(v.z));
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
    float lightrad = 1500.0;
//    vec3 lightpos = cam_pos+vec3(cos(PI/4.0 + framecount*lightspeed)*lightrad,
//                         3000.0,
//                         sin(PI/4.0 + framecount*lightspeed)*lightrad);
    vec3 lightoffset = normalize(vec3(vpn.x, 0.0, vpn.z))*0.05;
    vec3 lightpos = cam_pos + lightoffset;
    cam_light_pos = lightpos;
    //vec3 lightpos = cam_pos + u*0.1 + v*0.25; // + normalize(vpn)*2.0; // + u*2.0 + v*2.0;

    //vec3 lightpos = normalize(cam_pos*vec3(-1.0, 1.0, 1.0)) * 500.0;
    //vec3 lightpos =  vec3(400.0,400.0,400.0);
    
    //vec3 lightpos = cam_pos + normalize(vpn)*1.0 + u*0.01;

    // Raymarching.
    const vec3 e=vec3(0.02,0,0);
    const float maxd=1000.0; //Max depth
    vec2 d=vec2(0.01,0.0);
    vec3 c,p,N;

    float f=0.0001; // near plane?
    
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
            //d = vec2(1000.0, 1.0);
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


    //vec3 glowcol_miss = rainbow2_gradient(AO*1.0);
    //vec3 glowcol = rainbow2_gradient(AO*1.0); //vec3(1.0, 1.0, 1.0);
    //c =  rainbow2_gradient(AO*1.0);


    float cam_dist = length(cam_pos - p);
    vec3 spher = to_spherical(reflect(normalize(cam_pos-p), N ));
    vec2 uv = spher.xy / vec2(2.0*PI, PI);
    uv.y = 1.0 - uv.y;
    vec3 texcol = texture2D(texture, uv).rgb;
    //texcol = sun(texcol, normalize(cam_pos-p), normalize(p-lightpos));
    //texcol = iqfog(texcol, cam_dist, normalize(p-cam_pos), normalize(p-lightpos));
    
    
    vec3 stepbri = vec3(nsteps/float(MAX_RAY_STEPS));
    
    //c = prim_color(p/DSCALE, d.y);
    c = remnantx_color(p/DSCALE, 10.0);
    //c = rainbow2_gradient(p/DSCALE); 
    //c = p / DSCALE;
    //c = stepbri;
    //vec3 glow = vec3(stepbri) * c;
    vec3 glow =  c * stepbri * glow_intensity;
    vec3 glow_miss = glow;
    //vec3 glow_miss = c * stepbri * glow_intensity;

    
    if (f < maxd) {
        vec3 ambient = vec3(0.05, 0.04, 0.03);
        float amb_shad =0.2;
        float amb_lamb = 0.0;

        //vec3 lightdir = camp_pos-p;
        vec3 lightdir = normalize(lightpos - p);
        vec3 spec = specular(N, lightdir, normalize(cam_pos-p));

        //vec3 cam_dist_sc = vec3(cam_dist/ 256.0);
        float b=dot(N,lightdir);
        
        vec3 phong;
        if (b < 0.0) {
            phong = vec3(0.0);
        }
        else {
            phong =  vec3((b + pow(b,64.0))); // * (1.0-f*0.005));
        }
        
        //vec3 phong =  vec3((b + pow(b,8.0))); // * (1.0-f*0.005));
        vec3 lamb = amb_lamb + (1.0 - amb_lamb) * lambert(p, N, lightpos);
        //vec3 lamb = lambert(p, N, lightpos);
        float ldist = distance(p, lightpos);
        float shad = amb_shad + (1.0 - amb_shad) * iqsoftshadow(p, normalize(lightpos-p), 0.01, ldist, 16.0);
        //float shad = iqsoftshadow(p, normalize(lightpos-p), 0.1, 300.0, 32.0);
        //float shad = iqshadow(p, normalize(lightpos-p), 0.01, 300.0);

        //vec2 ls = distance_to_obj(cam_light_pos);


        vec3 fc = (
                //texcol
                //mix(texcol, c, 1.0)
                c 
                + mix(lamb, spec, diff_spec)
                //+ lamb * 0.5
                + phong * 1.5
                //+ n
                //+ vec3(1.0/ls.x)
                //+ vec3(distance(cam_light_pos, p));
                //+ LightSource(cam_light_pos-cam_pos, vpn, d.x)
                //+ LightSource(cam_light_pos-cam_pos, vpn, d.x)
                ) * 1.0 
                * shad
                * AO
                + ambient
                ;
       
       //fc = mix(vec3(0.0,0.0,0.0), fc, min(exp(-d.x+1.5), 1.0)); 
       //fc += vec3(pow(abs(d.y), 2.)) * vec3(.02, .04, .1);
        //vec3 fc = vec3(AO);
        //vec3 fc = vec3(phong+lamb+c*(1.0-stepbri))*0.25 * AO * shad;
        fc += glow;
        //fc = iqfog(fc, cam_dist, normalize(cam_pos-p), normalize(p-lightpos));
        
        // vignette
        fc *= 1. - dot(vPos, vPos);
        
        fc = pow(fc, vec3(gamma));
        gl_FragColor = vec4(fc, 1.0);
    }
    else {
        vec3 spher2 = to_spherical(normalize(cam_pos-p));
        vec2 uv2 = (spher2.xy / vec2(2.0*PI, PI));
        //uv2.y = 1.0 - uv2.y;
        vec3 texcol2 = texture2D(texture, uv2.xy).rgb;
        //vec3 bgcol = texcol2; //rainbow2_gradient(1.0);
        vec3 bgcol = vec3(0.0,0.0,0.0);
        //bgcol = sun(bgcol, normalize(p-cam_pos), normalize(p-lightpos));
        //bgcol = iqfog(bgcol, cam_dist, normalize(cam_pos-p), normalize(p-lightpos));
        bgcol += glow;
        //bgcol += glow_miss;
       // bgcol = D3DX_SRGB_to_FLOAT(bgcol);
        bgcol = pow(bgcol, vec3(gamma));
        gl_FragColor=vec4(bgcol.rgb,1.0); //background color
    }

}
