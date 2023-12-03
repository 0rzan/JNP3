module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module BST(Key : Comparable) =
  struct
    type btree =
      | Empty
      | Node of Key.t * btree * btree
      
    let rec insert x = function
      | Empty -> Node(x, Empty, Empty)
      | Node(value, left, right) ->
        if Key.compare x value < 0 then Node (value, insert x left, right)
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
          match (min_val, max_val) with
          | (Some min_v, Some max_v) ->
            Key.compare value min_v >= 0 && Key.compare value max_v <= 0
            && is_ordered (Some min_v) (Some value) left
            && is_ordered (Some value) (Some max_v) right
          | (None, Some max_v) ->
            Key.compare value max_v <= 0
            && is_ordered None (Some value) left
            && is_ordered (Some value) (Some max_v) right
          | (Some min_v, None) ->
            Key.compare value min_v >= 0
            && is_ordered (Some min_v) (Some value) left
            && is_ordered (Some value) None right
          | (None, None) ->
            is_ordered None (Some value) left
            && is_ordered (Some value) None right
      in is_ordered None None tree
  end

module Int_BST =
  BST(
    struct
      type t = int
      let compare = Int.compare
    end
  )