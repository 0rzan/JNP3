let rec bez_ost l = match l with
    | [] -> []
    | _ :: [] -> []
    | h :: t -> h :: bez_ost t
;;

let rec poczatki l = match l with
    | [] -> [[]]
    | _ -> l :: poczatki (bez_ost l)
;;