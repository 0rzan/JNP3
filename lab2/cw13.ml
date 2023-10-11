let rec co_drugi1 l = match l with
    | h1 :: (h2 :: t) -> h1 :: co_drugi1 t
    | _ -> []
;;

let co_drugi2 l =
    let (res, _) = 
        List.fold_left
            (fun (acc, b) x -> 
                if b then (x :: acc, not b)
                else (acc, not b)
            )
            ([], true)
            l
    in List.rev res
;;