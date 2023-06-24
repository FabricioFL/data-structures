namespace DataStructures.Structures.LinkedList

module LinkedList =

    type LinkedList<'T> =
        | Empty
        | Node of 'T * LinkedList<'T>

    let addFront value list =
        Node(value, list)

    let isEmpty list =
        match list with
        | Empty -> true
        | _ -> false

    let rec printList list =
        match list with
        | Empty -> ()
        | Node(value, next) ->
            printf "%A " value
            printList next
