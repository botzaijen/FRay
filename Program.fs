﻿let rnd = new System.Random()

type Color = {r:float32; g:float32; b:float32 }
type IntColor = {ir:int; ig:int; ib:int }

type Vec3 =
    struct
        val x:float32
        val y:float32
        val z:float32
        new(x, y, z) = {x = x; y = y; z = z}
        new(v:Vec3) = {x = v.x; y = v.y; z = v.z}
        override v.ToString() = sprintf "[%f, %f, %f]" v.x v.y v.z
    end

    static member (+) (a:Vec3, b:Vec3) = Vec3(a.x + b.x, a.y + b.y, a.z + b.z)
    static member (-) (a:Vec3, b:Vec3) = Vec3(a.x - b.x, a.y - b.y, a.z - b.z)
    static member (~-) (v:Vec3) = Vec3(-v.x, -v.y, -v.z)
    static member (*) (a:Vec3, b:Vec3) = Vec3(a.x * b.x, a.y * b.y, a.z * b.z)
    static member (*) (s:float32, b:Vec3) = Vec3(s * b.x, s * b.y, s * b.z)
    static member (*) (a:Vec3, s:float32) = Vec3(a.x * s, a.y * s, a.z * s)
    static member (/) (a:Vec3, s:float32) = Vec3(a.x / s, a.y / s, a.z / s)

let sqLength (v:Vec3) = v.x*v.x + v.y*v.y + v.z*v.z
let length (v:Vec3) = v |> sqLength |> sqrt
let normalize (v:Vec3) = 
    let k = 1.0f / (length v)
    v*k
let dot (a:Vec3) (b:Vec3) = a.x*b.x + a.y*b.y + a.z*b.z
let cross (a:Vec3) (b:Vec3) = 
    Vec3(a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x)
let colorFromVec3 (v:Vec3) : Color = {r=v.x; g=v.y; b=v.z}
let addColors (c1:Color) (c2:Color) = {r=c1.r+c2.r; g=c1.g+c2.g; b=c1.b+c2.b}
let mulColorF (f:float32) (c:Color) = {r=f*c.r; g=f*c.g; b=f*c.b}
let mulColorV (v:Vec3) (c:Color) = {r=v.x*c.r; g=v.y*c.g; b=v.z*c.b}
let colorToIntColor (c:Color) = { ir=(255.99f * c.r |> int ); ig=(255.99f * c.g |> int); ib=(255.99f * c.b |> int)}
let randomInUnitSphere () = 
    let x1 = (float32 (rnd.NextDouble()))
    let a = 1.0f - x1*x1
    let x2 = (float32 (rnd.NextDouble())) * sqrt(a)
    let b = a - x2*x2
    let x3 = (float32 (rnd.NextDouble())) * sqrt(b)
    let XYZ = [x1; x2; x3] |> List.map (fun x -> (rnd.Next(), x)) |> List.sortBy fst |> List.map (fun (x,y) -> y)
    Vec3(XYZ.[0], XYZ.[1], XYZ.[2])

type Ray = 
    struct
        val origin:Vec3
        val direction:Vec3
        new(A,B) = {origin=A; direction=B}
    end

let pointAt (r:Ray) (t:float32) = Vec3(r.origin + t*r.direction)

let lerp (source:Vec3) t (dest:Vec3) = (1.0f-t)*source + t*dest
type LambertianMaterial = { albedo:Vec3 }
type MetalMaterial = {albedo:Vec3; fuzz:float32}
type DielectricMaterial = {albedo:Vec3; refractiveIndex:float32}
type Material = 
    | Lambertian of LambertianMaterial
    | Metal of MetalMaterial
    | Dielectric of DielectricMaterial

type HitRecord = 
    struct 
        val t:float32
        val p:Vec3
        val normal:Vec3
        val material:Material
        new(t, p, n, m) = {t = t; p = p; normal = n; material = m}
    end

let reflect (v:Vec3) (normal:Vec3) = 
    v - 2.0f * dot v normal * normal

let refract (v:Vec3) (normal:Vec3) (n_ratio:float32) = 
    let I = normalize(v)
    let cosI =  -(dot I normal)
    let sinT2 = n_ratio * n_ratio * (1.0f - cosI * cosI)
    match sinT2 > 1.0f with
    | true -> None
    | false -> 
        let cosT = sqrt(1.0f - sinT2)
        Some (n_ratio * I + (n_ratio * cosI - cosT) * normal)

let schlick (cosine:float32) (ref_idx:float32) =
    let r0 = (1.0f - ref_idx) / (1.0f + ref_idx)
    let r1 = r0*r0
    r1 + (1.0f - r1)*(1.0f - cosine)**5.0f

let scatter (r:Ray) (record:HitRecord) = 
    match record.material with
    | Lambertian m ->
        let target = record.p + record.normal + randomInUnitSphere()
        let scattered = Ray(record.p, (target - record.p))  
        Some (m.albedo, scattered)
        
    | Metal m ->
        let reflected = reflect (normalize r.direction) record.normal
        let scattered = Ray(record.p, reflected + m.fuzz*randomInUnitSphere())
        match dot scattered.direction record.normal > 0.0f with
        | true -> Some (m.albedo, scattered)
        | false -> None
    | Dielectric m ->
        //let reflected = reflect (normalize r.direction) record.normal
        
        let (refracted, cosine) = 
            match dot r.direction record.normal > 0.0f with
            | true ->
                (refract r.direction (-record.normal) m.refractiveIndex,
                    m.refractiveIndex * dot r.direction record.normal / length r.direction )
            | false ->
                (refract r.direction record.normal (1.0f/m.refractiveIndex), 
                    -(dot r.direction record.normal) / length r.direction)
        match refracted with
            | Some t -> 
                let reflectProbability = schlick cosine m.refractiveIndex
                match float32 (rnd.NextDouble()) < reflectProbability with
                | true ->
                    let reflected = reflect (normalize r.direction) record.normal
                    Some (m.albedo, Ray(record.p, reflected))
                | false ->
                    Some (m.albedo, Ray(record.p, t))
            | None -> 
                let reflected = reflect (normalize r.direction) record.normal
                Some (m.albedo, Ray(record.p, reflected))

type Sphere = {center:Vec3; radius:float32; material:Material}
type SceneObject =
    | SphereObject of Sphere 
    | Cube

//let scatter (r:Ray) 
type MinMax = {min:float32; max:float32}
let hitSphere (s:Sphere) (m:MinMax) (r:Ray) = 
    let oc = r.origin - s.center
    // t^2*a+t*b+c = 0 use discriminant to test for hit
    // if > 0 we have two roots, if = 0 one root, if < 0 no hit
    let a = dot r.direction r.direction
    let b = 2.0f * dot oc r.direction
    let c = dot oc oc - s.radius * s.radius
    let discriminant = b*b - 4.0f*a*c
    let {min = tmin; max = tmax} = m
    let r1 = ((-b - sqrt(discriminant)) / (2.0f * a))
    let r2 = ((-b + sqrt(discriminant)) / (2.0f * a))
    let hit = ((discriminant > 0.0f), (r1 < tmax) && (r1 > tmin), (r2 < tmax) && (r2 > tmin) )
    // solve for t and return real solution
    match hit with
    | (true, true, _) ->
            let p = pointAt r r1
            let norm = (p - s.center) / s.radius
            Some (HitRecord(r1, p, norm, s.material)) 
    | (true, false, true) ->
            let p = pointAt r r2
            let norm = (p - s.center) / s.radius
            Some (HitRecord(r2, p, norm, s.material)) 
    | (_, _, _) -> 
            None
let hitSceneObject (m:MinMax) (r:Ray) (hitable:SceneObject)=
    match hitable with
    | SphereObject s -> hitSphere s m r
    | Cube -> None

let rec colorRay (hitables:SceneObject list) (r:Ray) (depth:int)= 
    let mm = {min=0.0001f; max=System.Single.MaxValue}
    let boundray = hitSceneObject mm r
    let optionMin (x:HitRecord option) (y:HitRecord option) =
        match x, y with
        | (Some x', Some y') -> if x'.t < y'.t then Some x' else Some y'
        | (Some x', None) -> Some x'
        | (None, Some y') -> Some y'
        | (None, None) -> None
    //let hit = hitables |> List.map boundray |> List.fold optionMin None
    let rec getClosest l acc = 
        match l with
        | [] -> acc
        | x::tail -> getClosest tail (optionMin (boundray x) acc)
    let hit = getClosest hitables None
    match hit with
        | Some hrec -> 
            let scatterResult = scatter r hrec
            match scatterResult with
            | Some (att, scattered) when depth < 50 ->
                (mulColorV att (colorRay hitables scattered (depth+1)))
            | Some _ | None -> 
                {r=0.0f; g=0.0f; b=0.0f} 
        | None -> 
            let norm_dir = normalize r.direction
            let t = 0.5f*(norm_dir.y + 1.0f)
            let cv = lerp (Vec3(1.0f, 1.0f, 1.0f)) t (Vec3(0.5f, 0.7f, 1.0f))
            (colorFromVec3 cv) 

type Camera(lookFrom:Vec3, lookAt:Vec3, vup:Vec3, vfov:float, aspect:float) = 
    let theta = vfov*System.Math.PI/180.0
    let half_height = float32 (System.Math.Tan (theta/2.0))
    let half_width = float32 aspect * half_height
    let w = normalize (lookFrom - lookAt)
    let u = normalize (cross vup w)
    let v = cross w u
    member this.origin = lookFrom
    member this.lowerLeftCorner =
        lookFrom - half_width*u - half_height*v - w
    member this.horizontal = 2.0f*half_width * u
    member this.vertical = 2.0f*half_height * v
    member this.getRay (u:float32) (v:float32) = Ray(this.origin, this.lowerLeftCorner + u*this.horizontal + v*this.vertical - this.origin) 

[<EntryPoint>]
let main argv = 
    let (nx,ny,ns) = (200, 100, 100)
    printfn "P3\n %d %d \n255" nx ny
    let cam = Camera(Vec3(-2.0f, 2.0f, 1.0f), Vec3(0.0f, 0.0f, -1.0f), Vec3(0.0f, 1.0f, 0.0f), 30.0, (float nx)/(float ny))
    //let cam = Camera(Vec3(0.0f, 0.0f, -1.0f), Vec3(0.0f, 0.0f, -1.0f), Vec3(0.0f, 1.0f, 0.0f), 90.0, (float nx)/(float ny))
    let world = [
        SphereObject {center=Vec3(0.0f, 0.0f, -1.0f); radius=0.5f; material=Lambertian{albedo=Vec3(0.1f, 0.2f, 0.5f)}};
        SphereObject {center=Vec3(0.0f, -100.5f, -1.0f); radius=100.0f; material=Lambertian{albedo=Vec3(0.8f, 0.8f, 0.0f)}};
        SphereObject {center=Vec3(1.0f, 0.0f, -1.0f); radius=0.5f; material=Metal{albedo=Vec3(0.8f, 0.6f, 0.2f); fuzz=0.0f}};
        SphereObject {center=Vec3(-1.0f, 0.0f, -1.0f); radius=0.5f; material=Dielectric{albedo=Vec3(0.99f, 0.99f, 0.99f); refractiveIndex=1.5f}};
        SphereObject {center=Vec3(-1.0f, 0.0f, -1.0f); radius=(-0.45f); material=Dielectric{albedo=Vec3(0.99f, 0.99f, 0.99f); refractiveIndex=1.5f}};
        ]
    for j in [ny-1 .. -1 .. 0] do
        for i in [0 .. nx-1] do
            let rndlist = [for _ in [1..ns] do yield (rnd.NextDouble(), rnd.NextDouble())]
            let col = rndlist 
                    |> List.map (fun (x,y) -> (((float32 i) + (float32 x)) / (float32  nx), ((float32 j) + (float32 y)) / (float32 ny))) 
                    |> List.map (fun (u,v) -> colorRay world (cam.getRay u v) 0) 
                    |> List.fold addColors ({r=0.0f; g=0.0f; b=0.0f}) 
                    |> fun c -> {r=sqrt(c.r/(float32 ns)); g=sqrt(c.g/(float32 ns)); b=sqrt(c.b/(float32 ns))} 
            let icol = col |> colorToIntColor
            printfn "%d %d %d" icol.ir icol.ig icol.ib
    0 // return an integer exit code
