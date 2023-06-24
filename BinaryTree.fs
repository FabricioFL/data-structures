namespace DataStructures.BinaryTree

module BinaryTree =

    type BinaryTree<'T> = 
    | Empty
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>

    let rec insert  value tree =
        match tree with
        | Empty -> Node(value, Empty, Empty)
        | Node(v, left, right) when value < v -> Node(v, insert value left, right)
        | Node(v, left, right) when value > v -> Node(v, left, insert value right)
        | _ -> tree

    let rec contains value tree =
        match tree with
        | Empty -> false
        | Node(v, left, right) when value < v -> contains value left
        | Node(v, left, right) when value > v -> contains value right
        | Node(v, _, _) -> value = v

