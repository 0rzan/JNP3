open Picture

type single_transform =
| Translate of vector
| Rotate of r

val translate_point : point -> vector -> point
val rotate_point : point -> r -> point
val s_transform_point : point -> single_transform -> point
val s_transform_vec : vector -> single_transform -> vector

type transform = single_transform list
val sum : transform -> transform -> transform
val id : transform
val translate : vector -> transform
val rotate : r -> transform
val full_circle : r

val trpoint : transform -> point -> point
val trvec : transform -> vector -> vector
val transform : transform -> picture -> picture