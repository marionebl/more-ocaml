(* From 1. Unravelling "Fold" *)
type 'a tree =
    | Lf
    | Br of 'a * 'a tree * 'a tree