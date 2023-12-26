open Picture
open Transform

type state = {
  pic : picture;
  transforms : transform;
  stack : r option Stack.t;
  cur_point : point;
  is_defined_point : bool;
  start_point : point;
  is_defined_start_point : bool;
  is_error : bool;
}

val parse_input : in_channel -> picture * bool