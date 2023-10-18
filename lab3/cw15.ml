let plateau l = match l with
    | [] -> (0, 0)
    | h :: t ->
        let (hmax, dmax, hcur, dcur) =
            List.fold_left
                (fun (hmax, dmax, hcur, dcur) x ->
                    if x = hcur then (
                        if dmax < dcur + 1 then (hcur, (dcur + 1), hcur, (dcur + 1))
                        else (hmax, dmax, hcur, (dcur + 1))
                    )
                    else (hmax, dmax, x, 1)
                )
                (h, 1, h, 1)
                t
        in (hmax, dmax)
;;