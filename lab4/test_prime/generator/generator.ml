let limit = Sys.argv.(1)

let gen l x = 
    let rec gen_aux acc cur x = match cur with
    | _ -> if cur > x then acc
           else if Is_prime.is_prime acc cur then
             gen_aux (acc @ [cur]) (cur + 2) x
           else
             gen_aux acc (cur + 2) x
    in gen_aux l (List.hd(List.rev(l)) + 2) x

let small_primes = [2;3;5;7;11;13;17;19]

let tmp = gen small_primes (int_of_string limit)

let int_list_to_string l =
  "[" ^ ( List.fold_left( fun acc x -> acc ^ string_of_int x ^ ";" ) "" l) ^ "]"

let string_list = int_list_to_string tmp

let () = Printf.printf "let l = %s" string_list