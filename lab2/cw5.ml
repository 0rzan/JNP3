let drugi_z_pierwszym x = match x with
    | a :: b :: _ -> (
        match (a, b) with
        | (_ :: x :: _, h :: _) -> h + x
        | _ -> invalid_arg "invalid arg"
    )
    | _ -> invalid_arg "invalid arg"
;;