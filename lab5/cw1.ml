type number =
  | Whole of int
  | Frac of float

let is_whole f =
  let fractional, _ = Float.modf f in
  Float.equal fractional 0.0

let add x y =
  match (x, y) with
  | (Whole xp, Whole yp) -> Whole (xp + yp)
  | (Frac xp, Whole yp) ->
      if is_whole (xp +. float_of_int yp) then
        Whole (int_of_float (xp +. float_of_int yp))
      else
        Frac (xp +. float_of_int yp)
  | (Whole xp, Frac yp) ->
      if is_whole (float_of_int xp +. yp) then
        Whole (int_of_float (float_of_int xp +. yp))
      else
        Frac (float_of_int xp +. yp)
  | (Frac xp, Frac yp) ->
      if is_whole (xp +. yp) then
        Whole (int_of_float (xp +. yp))
      else
        Frac (xp +. yp)
;;