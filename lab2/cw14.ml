let rec aplikuj1 l a = match l with
    | h :: t -> h (aplikuj1 t a)
    | _ -> a
;;

let aplikuj2 l a =
    List.fold_right (fun f x -> (f x)) l a
;;