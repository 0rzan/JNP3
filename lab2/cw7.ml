let rec zrob n =
    if n = 0 then [] else n :: zrob (n - 1)
;;

let rec ogony l = match l with
    | [] -> [[]]
    | _ :: t -> l :: ogony t
;;

let rec append l1 l2 = match l1 with
    | [] -> l2
    | h :: t -> h :: append t l2
;;

let odwracanie l =
    let rec pom a l = match l with
    | [] -> a
    | h :: t -> pom (h :: a) t
    in pom [] l
;;