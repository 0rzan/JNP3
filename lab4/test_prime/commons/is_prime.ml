let is_prime l x = 
    let pierw = int_of_float (sqrt (float_of_int x)) in
    let rec is_prime_aux l x acc = match l with
    | [] -> if x mod acc == 0 then false
            else if acc > pierw then true
            else is_prime_aux l x (acc + 1)
    | h::t -> if x mod h == 0 then false
              else if h > pierw then true         
              else is_prime_aux t x (h + 1)
    in is_prime_aux l x 2
;; 