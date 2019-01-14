type Color = {r:float32; g:float32; b:float32 }
type IntColor = {r:int; g:int; b:int }
let colorToIntColor (c:Color) = { r=(255.99f * c.r |> int ); g=(255.99f * c.g |> int); b=(255.99f * c.b |> int)}

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

let sqLength (v:Vec3) = v.x*v.x + v.y*v.y + v.z*v.z
let length (v:Vec3) = v |> sqLength |> sqrt
let normalize (v:Vec3) = 
    let k = 1.0f / (length v)
    v*k
let dot (a:Vec3) (b:Vec3) = a.x*b.x + a.y*b.y + a.z*b.z

type Ray = 
    struct
        val origin:Vec3
        val direction:Vec3
        new(A,B) = {origin=A; direction=B}
    end
let pointAt (r:Ray, t:float32) = Vec3(r.origin + t*r.direction)

let lerp (source:Vec3) t (dest:Vec3) = (1.0f-t)*source + t*dest
type Sphere = {center:Vec3; radius:float32; color:Color}
let hitSphere (s:Sphere)(r:Ray) = 
    let oc = r.origin - s.center
    // t^2*a+t*b+c = 0 use discriminant to test for hit
    let a = dot r.direction r.direction
    let b = 2.0f * dot oc r.direction
    let c = dot oc oc - s.radius * s.radius
    let discriminant = b*b - 4.0f*a*c
    if discriminant > 0.0f then
        true
    else 
        false

let colorRay (r:Ray) = 
    let sphere = {center=Vec3(0.0f, 0.0f, -1.0f); radius=0.5f; color={r=1.0f; g=0.0f; b=0.0f}}
    match hitSphere sphere r with
    | true -> colorToIntColor sphere.color
    | false -> 
        let norm_dir = normalize r.direction
        let t = 0.5f*(norm_dir.y + 1.0f)
        let cv = lerp (Vec3(1.0f, 1.0f, 1.0f)) t (Vec3(0.5f, 0.7f, 1.0f))
        colorToIntColor {r=cv.x; g=cv.y; b=cv.z}

[<EntryPoint>]
let main argv = 
    let (nx,ny) = (200, 100)
    printfn "P3\n %d %d \n255" nx ny
    let lowerLeftCorner = Vec3(-2.0f, -1.0f, -1.0f)
    let horizontal = Vec3(4.0f, 0.0f, 0.0f)
    let vertical = Vec3(0.0f, 2.0f, 0.0f)
    let origin = Vec3(0.0f, 0.0f, 0.0f)
    for j in [ny-1 .. -1 .. 0] do
        for i in [0 .. nx-1] do
            let (u,v) = ((float32 i) / (float32 nx), (float32 j) / (float32 ny))
            let r = Ray(origin, lowerLeftCorner + u*horizontal + v*vertical)
            let icol = colorRay r
            printfn "%d %d %d" icol.r icol.g icol.b
    0 // return an integer exit code
