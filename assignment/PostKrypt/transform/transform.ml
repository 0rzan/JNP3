open Picture

let deg_of_rad r = r *. acos (-1.0) /. 180.0

type single_transform =
| Translate of vector
| Rotate of r

let translate_point p v =
  { coordinates = (p.coordinates +| v.direction.coordinates) }

let rotate_point p r =
  let x, y = p.coordinates in
  let x' = x *. cos (deg_of_rad r) -. y *. sin (deg_of_rad r) in
  let y' = x *. sin (deg_of_rad r) +. y *. cos (deg_of_rad r) in
  { coordinates = (x', y') }

let s_transform_point p t =
  match t with
  | Translate v -> translate_point p v
  | Rotate r -> rotate_point p r

let s_transform_vec v t =
  match t with
  | Translate _ -> v
  | Rotate r -> { direction = rotate_point v.direction r }

type transform = single_transform list

let id = []
let sum t1 t2 = t1 @ t2

let translate v = [Translate v]
let rotate r = [Rotate r]
let full_circle = 360.0

let trpoint t p =
  List.fold_left s_transform_point p t

let trvec t v =
  List.fold_left s_transform_vec v t

let trline t l =
  let (p1, p2) = l in
  (trpoint t p1, trpoint t p2)

let transform t pic =
  { lines = List.map (fun l -> trline t l) pic.lines }
