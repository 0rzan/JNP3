type 'a seq = 
| Empty 
| NonEmpty of 'a * 'a seq

let rec insert l x = match l with
| Empty -> NonEmpty(x, Empty)
| NonEmpty (a, seq) -> 
    if x <= a then
      NonEmpty(x, NonEmpty(a, seq))
    else
      NonEmpty(a, insert seq x)

let sort l =
  let rec sort_aux l acc = match l with
  | Empty -> acc
  | NonEmpty (a, seq) -> sort_aux seq (insert acc a)
  in sort_aux l Empty

let l = NonEmpty(7, NonEmpty(5, NonEmpty(1, NonEmpty(2, Empty))));;
let l_sorted = sort l
;;