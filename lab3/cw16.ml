let rec insert l x = match l with
    | [] -> [x]
    | h :: t ->
        if x <= h then
            x :: l
        else
            h :: (insert t x)
;;

let sort l =
    List.fold_left insert [] l
;;
