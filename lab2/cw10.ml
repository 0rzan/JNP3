let odwracanie l =
    let rec pom a l = match l with
    | [] -> a
    | h :: t -> pom (h :: a) t
    in pom [] l
;;

let rec odwroc_wszystkie l = match l with
    | [[]] -> [[]]
    | h :: t -> (odwracanie h) :: odwroc_wszystkie t
;;