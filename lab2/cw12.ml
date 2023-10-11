let parzyste l = 
    List.filter (fun x -> x mod 2 = 0) l
;;

let rec map f l = match l with
    | [] -> []
    | h :: t -> f h :: map f t
;;

let polowki l =
    map (fun x -> x / 2) (parzyste l)
;;