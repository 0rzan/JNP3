open Test_prime

let arg_as_int i = int_of_string Sys.argv.(i)

let wynik x = match x with 
| _ -> if x <= 0 then 
         "Liczba niedodatnia."
       else if Prime.prime x then
         "Liczba pierwsza."
       else
         "Liczba złożona."
        
let () = print_endline (wynik (arg_as_int 1))