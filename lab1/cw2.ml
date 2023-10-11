let rec fib_wyk x =
    if x = 0 then 0
    else if x = 1 then 1
    else fib_wyk (x - 1) + fib_wyk (x - 2);;

let fib_lin x =
    if x = 0 then 0
    else
        let rec pom a b n =
            if n = x then b
            else pom b (a + b) (n + 1)
        in
        pom 0 1 1;;