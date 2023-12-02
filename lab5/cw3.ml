type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree

let rec insert x = function
  | Empty -> Node(x, Empty, Empty)
  | Node(value, left, right) ->
    if x < value then Node (value, insert x left, right)
    else Node (value, left, insert x right)

let rec read_tree tree =
  let rec traverse acc = function
  | Empty -> acc
  | Node (value, left, right) ->
    let acc_left = traverse acc left in
    let acc_value = value :: acc_left in
    traverse acc_value right
  in List.rev(traverse [] tree)

let build_tree lst =
  List.fold_left (fun acc x -> insert x acc) Empty lst

let sort_list lst = read_tree(build_tree lst)

module BST =
  struct
    type 'a btree =
      | Empty
      | Node of 'a * 'a btree * 'a btree
    let rec insert x = function
      | Empty -> Node(x, Empty, Empty)
      | Node(value, left, right) ->
        if x < value then Node (value, insert x left, right)
        else Node (value, left, insert x right)
    let build lst =
      List.fold_left (fun acc x -> insert x acc) Empty lst
    let rec read tree =
      let rec traverse acc = function
        | Empty -> acc
        | Node (value, left, right) ->
          let acc_left = traverse acc left in
          let acc_value = value :: acc_left in
          traverse acc_value right
      in List.rev(traverse [] tree)
    let is_bst tree =
      let rec is_ordered min_val max_val = function
        | Empty -> true
        | Node (value, left, right) ->
          value >= min_val && value <= max_val
          && is_ordered min_val value left
          && is_ordered value max_val right
      in is_ordered min_int max_int tree
  end