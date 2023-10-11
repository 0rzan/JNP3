let rec combine_lists a b a_list =
    match a_list with
    | [] -> []
    | h :: t -> 
        if h < a then b :: combine_lists a b t
        else combine_lists a b t
;;