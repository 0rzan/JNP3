module ArithmeticExpr =
  struct
    module CharMap = Map.Make(Char)

    type t = 
      | Int of int
      | Plus of t * t
      | Times of t * t
      | Neg of t
      | Var of char

    let parse s =
      let len = String.length s in
      let stack = Stack.create () in
      let is_digit c = c >= '0' && c <= '9' in

      for i = 0 to len - 1 do
        match s.[i] with
        | '+' ->
          Stack.push (Plus (Stack.pop stack, Stack.pop stack)) stack
        | '-' ->
          Stack.push (Neg (Stack.pop stack)) stack
        | '*' ->
          Stack.push (Times (Stack.pop stack, Stack.pop stack)) stack
        | _ as c ->
          if is_digit c then
            Stack.push (Int (int_of_string (String.make 1 c))) stack
          else if c >= 'a' && c <= 'z' then
            Stack.push (Var c) stack
      done;

      Stack.pop stack

    let rec calcf f d = 
      match d with
        | Int n -> n
        | Plus (e1, e2) -> calcf f e1 + calcf f e2
        | Times (e1, e2) -> calcf f e1 * calcf f e2
        | Neg e -> -1 * calcf f e
        | Var c -> f c

    let rec calc m d =
      match d with 
        | Int n -> n
        | Plus (e1, e2) -> calc m e1 + calc m e2
        | Times (e1, e2) -> calc m e1 * calc m e2
        | Neg e -> -1 * calc m e
        | Var c -> CharMap.find c m 

    let compare e1 e2 = 
      compare (calcf (fun x -> 0) e1) (calcf (fun x -> 0) e2)
  end

module ExprBST = BST(ArithmeticExpr)

let expr1 = "x3+"
let expr2 = "123+*-"
let expr3 = "0123+*+"

let bst = ExprBST.(Empty |> insert (ArithmeticExpr.parse expr1) |> insert (ArithmeticExpr.parse expr2) |> insert (ArithmeticExpr.parse expr3))
let bst_sorted = ExprBST.read bst
let bst_sorted_to_int = List.map (ArithmeticExpr.calcf (fun x -> 0)) bst_sorted