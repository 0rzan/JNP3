open Picture
open Transform

let read_file channel =
  let rec read_words acc =
    try
      let line = input_line channel in
      let words = String.split_on_char ' ' line in
      let filtered_words = List.filter (fun w -> w <> "") words in
      read_words (List.rev_append filtered_words acc)
    with End_of_file ->
      close_in channel;
      List.rev acc
  in
  read_words []

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

let pop_two state =
  match (Stack.pop state.stack, Stack.pop state.stack) with
  | (Some a, Some b) -> (Some b, Some a)
  | _ -> (None, None)

let execute_push i state =
  Stack.push i state.stack;
  state

let execute_operation op state =
  let (elem1, elem2) = pop_two state in
  match (elem1, elem2) with
  | (Some element1, Some element2) -> execute_push (Some (op element1 element2)) state
  | _ -> { state with is_error = true }

let execute_add state = execute_operation (+.) state

let execute_sub state = execute_operation (-.) state

let execute_mul state = execute_operation ( *. ) state

let execute_div state =
  let (elem1, elem2) = pop_two state in
  match (elem1, elem2) with
  | (Some element1, Some element2) -> 
    if element2 = 0. then
      { state with is_error = true }
    else
      execute_push (Some (element1 /. element2)) state
  | _ -> { state with is_error = true }

let execute_move_to state =
  match pop_two state with
  | (Some element1, Some element2) ->
      let transformed_point = trpoint state.transforms (point (element1, element2)) in
      {
        state with
        cur_point = transformed_point;
        is_defined_point = true;
        start_point = transformed_point;
        is_defined_start_point = true;
      }
  | _ -> { state with is_error = true }

let execute_line_to state =
  if not state.is_defined_point then
    { state with is_error = true }
  else
    let (elem1, elem2) = pop_two state in
    match (elem1, elem2) with
    | Some element1, Some element2 ->
        let transformed_point = trpoint state.transforms (point (element1, element2)) in
        {
          state with
          cur_point = transformed_point;
          pic =
            {
              lines =
                (state.cur_point, transformed_point) :: state.pic.lines;
            };
          is_defined_point = true;
        }
    | _ -> { state with is_error = true }

let execute_close_path state =
  if not state.is_defined_start_point || state.cur_point = state.start_point then
    { state with is_error = true }
  else
    {
      state with
      pic =
      {
        lines =
          (state.cur_point, state.start_point) :: state.pic.lines;
      };
      cur_point = state.start_point;
    }

let execute_translate state =
  let (elem1, elem2) = pop_two state in
  match (elem1, elem2) with
  | Some element1, Some element2 ->
      let translation = translate (vector (element1, element2)) in
      { state with transforms = translation @ state.transforms }
  | _ -> { state with is_error = true }

let execute_rotate state =
  match Stack.pop state.stack with
  | Some element1 -> { state with transforms = (rotate element1) @ state.transforms }
  | None -> { state with is_error = true }

let execute_set_error state = { state with is_error = true }

let is_integer s =
  try
    ignore (int_of_string s);
    true
  with
  | Failure _ -> false

let start_state = {
  pic = { lines = [] };
  transforms = id;
  stack = Stack.create ();
  cur_point = point (0., 0.);
  is_defined_point = false;
  start_point = point (0., 0.);
  is_defined_start_point = false;
  is_error = false;
}

let parse_input channel = 
  let input_list = read_file channel in
    let rec parse_input' input_list state =
      match input_list with
      | [] -> ({ lines = state.pic.lines }, state.is_error)
      | hd :: tl ->
          if is_integer hd then
            parse_input' tl (execute_push (Some (float_of_string hd)) state)
          else
            match hd with
            | "add" -> parse_input' tl (execute_add state)
            | "sub" -> parse_input' tl (execute_sub state)
            | "mul" -> parse_input' tl (execute_mul state)
            | "div" -> parse_input' tl (execute_div state)
            | "moveto" -> parse_input' tl (execute_move_to state)
            | "lineto" -> parse_input' tl (execute_line_to state)
            | "closepath" -> parse_input' tl (execute_close_path state)
            | "translate" -> parse_input' tl (execute_translate state)
            | "rotate" -> parse_input' tl (execute_rotate state)
            | _ -> parse_input' [] (execute_set_error state)
  in parse_input' input_list start_state