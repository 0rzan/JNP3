open Graphics
open Picture
open Inputpic

let draw_lines lines =
  open_graph " 800x600";
  let rec aux_draw_lines lines =
    match lines with
    | [] -> ()
    | ((x1, y1), (x2, y2)) :: rest ->
      moveto x1 y1;
      lineto x2 y2;
      aux_draw_lines rest
  in
  aux_draw_lines lines;
  ignore (wait_next_event [Button_down])

let prolog = "300 400 translate"
let epilog = "stroke showpage"
let error_msg = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"

let print_ps lines is_error =
  if is_error then
    print_endline (prolog ^ "\n\n" ^ error_msg ^ "\n\n" ^ epilog)
  else
    print_endline (prolog ^ "\n\n" ^ ps_string_of_rendering lines ^ "\n" ^ epilog)

let center x y rendering = 
  List.map (fun ((x1, y1), (x2, y2)) -> ((x1 + x, y1 + y), (x2 + x, y2 + y))) rendering

let n = ref 1
let d = ref false
let filename = ref ""

let speclist = [
  ("-n", Arg.Set_int n, "Value of n (default is 1)");
  ("-d", Arg.Unit (fun () -> d := true), "Draw the picture (default is false)")
]

let () =
  let usage_msg = "Usage: my_program [-n n] [-d] filename" in
  Arg.parse speclist (fun arg -> filename := arg) usage_msg;
  let n_value = !n in
  let draw = !d in
  let channel = open_in !filename in
  let (pic, is_error) = parse_input channel in
  if draw then
    if is_error then
      print_endline "Error: invalid input"
    else
      draw_lines (center 400 300 (render_scaled n_value pic))  
  else
    print_ps (render_scaled n_value pic) is_error