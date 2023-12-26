type r = float
type r2 = r * r

let ( +| ) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
let ( *| ) s (x, y) = (s *. x, s *. y)

let zero_r = 0.
let zero_r2 = (zero_r, zero_r)

(* Point *)
type point = { coordinates : r2 }
let point r2 = { coordinates = r2 }
let point_zero = { coordinates = zero_r2 }

let string_of_point p =
  let (a, b) = p.coordinates in
  string_of_float a ^ " " ^ string_of_float b

(* Vector *)
type vector = { direction : point }
let vector r2 = { direction = point r2 }

let string_of_vector v =
  let p = v.direction in
  "(" ^ string_of_point p ^ ")"

(* Line and Picture *)
type line = point * point
type picture = { lines : line list }

let string_of_line line =
  let (p1, p2) = line in
  "(" ^ string_of_point p1 ^ ", " ^ string_of_point p2 ^ ")"

let line r21 r22 = { lines = [(point r21, point r22)] }

let rectangle r1 r2 = 
  {
    lines = 
      [
        (point zero_r2, point (r1, zero_r));
        (point (r1, zero_r), point (r1, r2));
        (point (r1, r2), point (zero_r, r2));
        (point (zero_r, r2), point zero_r2)
      ]
  }

let baloon =
  let rect = rectangle 100. 100. in
  let diag = line (-100., -100.) zero_r2 in
  { lines = diag.lines @ rect.lines }

let ( +++ ) p1 p2 = { lines = p1.lines @ p2.lines }

let string_of_picture pic =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (acc ^ string_of_line h ^ "\n") t
  in
  aux "" pic.lines

(* Rendering *)
type int_line = (int * int) * (int * int)
type int_rendering = int_line list

let string_of_int_point p =
  let (a, b) = p in
  string_of_int a ^ " " ^ string_of_int b

let ps_string_of_rendering r =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (acc ^ string_of_int_point (fst h) ^ " moveto " ^ string_of_int_point (snd h) ^ " lineto\n") t
  in
  aux "" r

let point_to_int p =
  let (x, y) = p.coordinates in
  (int_of_float x, int_of_float y)

let scale_point i p =
  { coordinates = (float_of_int i) *| p.coordinates }

let scale_line i l =
  let (p1, p2) = l in
  (point_to_int (scale_point i p1), point_to_int (scale_point i p2))

let render_scaled i pic = List.map (scale_line i) pic.lines