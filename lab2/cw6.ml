let rec list_size1 l = match l with 
    | [] -> 0
    | h :: t -> 1 + list_size1 t
;;

let list_size2 l = 
    let rec pom a l = match l with
        | [] -> a
        | h :: t -> pom (a + 1) t
    in pom 0 l
;;

let rosnace n = 
    let rec pom a k =
        if k = n + 1 then a
        else pom (a @ [k]) (k + 1)
    in pom [] 1
;;

let rosnace2 n =
    let rec pom k =
        if k = n + 1 then []
        else k :: pom (k + 1)
    in pom 1
;;