let replace_char c =
  match c with
  | 'a' -> 'b'
  | 'b' -> 'a'
  | 'A' -> 'B'
  | 'B' -> 'A'
  | _ -> c

let modify_file input_file output_file =
  let ic = open_in input_file in
  let oc = open_out output_file in
  try
    while true do
      let line = input_line ic in
      let modified_line = String.map replace_char line in
      output_string oc (modified_line ^ "\n")
    done
  with End_of_file ->
    close_in ic;
    close_out oc

let () =
  if Array.length Sys.argv <> 3 then
    Printf.printf "Użycie: %s plik_wejsciowy plik_wyjsciowy\n" Sys.argv.(0)
  else
    let input_file = Sys.argv.(1) in
    let output_file = Sys.argv.(2) in
    modify_file input_file output_file