let rec sto_par_wyk x =
    if x mod 2 = 1 then 0
    else 1 + sto_par_wyk (x / 2);;

let sto_par_lin x =
    let rec pom a y =
        if y mod 2 = 1 then a
        else pom (a + 1) (y / 2)
    in pom 0 x;;