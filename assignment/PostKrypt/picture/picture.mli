type r = float
type r2 = r * r

val ( +| ) : r2 -> r2 -> r2
val ( *| ) : r -> r2 -> r2

val zero_r : r
val zero_r2 : r2

type point = { coordinates : r2 }
val point : r2 -> point
val point_zero : point
val string_of_point : point -> string

type vector = { direction : point }
val vector : r2 -> vector
val string_of_vector : vector -> string

type line = point * point
type picture = { lines : line list }
val string_of_line : line -> string

val line : r2 -> r2 -> picture
val rectangle : r -> r -> picture
val baloon : picture
val ( +++ ) : picture -> picture -> picture 
val string_of_picture : picture -> string

type int_line = (int * int) * (int * int)
type int_rendering = int_line list
val ps_string_of_rendering : int_rendering -> string
val render_scaled : int -> picture -> int_rendering