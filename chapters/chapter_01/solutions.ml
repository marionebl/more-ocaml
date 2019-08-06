(* The List module in Ocaml's Standard Library defines two intriguingly-named functions over lists: *)
let rec fold_left f a l =
    match l with
    | [] -> a
    | h::t -> fold_left f (f a h) t

let rec fold_right f l a =
    match l with
    | [] -> a
    | h::t -> f h (fold_right f t a)

(* We can define map simply as use of fold_right. Who would have thought that fold_right was the more
   fundamental function? *)
let map f l =
    fold_right (fun e a -> f e :: a) l []

(* [...] applying :: over an input list with fold_right is not very interesting, yielding a function
   which returns a copy of its input: *)
let copy l =
    fold_right List.cons l []

(* But if we supply an non-empty list as the initial value of the accumulator, we have the append function: *)
let append x y =
    fold_right List.cons x y

(* We can use a more complicated accumulator, such as a tuple. [...] we replicate the List.split function which,
   given a list of pairs, yields a pair of lists: *)
let split l =
    fold_right (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) l ([], [])