let rec aplikuj1 l a = match l with
    | h :: t -> aplikuj1 t (h a)
    | _ -> a
;;

let aplikuj2 l a =
    List.fold_left (fun x f -> (f x)) a l
;;