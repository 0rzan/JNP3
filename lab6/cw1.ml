type expr = 
| Int of int
| Plus of expr * expr
| Times of expr * expr
| Neg of expr

let parse s =
  let len = String.length s in
  let stack = Stack.create () in
  let rec get_digit c = int_of_string (String.make 1 c) in

  for i = 0 to len - 1 do
    match s.[i] with
    | '+' ->
      Stack.push (Plus (Stack.pop stack, Stack.pop stack)) stack
    | '-' ->
      Stack.push (Neg (Stack.pop stack)) stack
    | '*' ->
      Stack.push (Times (Stack.pop stack, Stack.pop stack)) stack
    | _ ->
      Stack.push (Int (get_digit s.[i])) stack
  done;

  Stack.pop stack

let rec calc d =
  match d with 
    | Int n -> n
    | Plus (e1, e2) -> calc e1 + calc e2
    | Times (e1, e2) -> calc e1 * calc e2
    | Neg e -> -1 * calc e
