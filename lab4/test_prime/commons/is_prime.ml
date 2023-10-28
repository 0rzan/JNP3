let is_prime l x = 
    let rec is_prime_aux l x acc = match l with
    | [] -> if x mod acc == 0 then false
            else if acc > int_of_float (sqrt (float_of_int x)) then true
            else is_prime_aux l x (acc + 1)
    | h::[] -> if x mod h == 0 then false
               else is_prime_aux [] x (h + 1)
    | h::t -> if x mod h == 0 then false
              else is_prime_aux t x acc
    in is_prime_aux l x 2
;; 