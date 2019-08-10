open Types

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

(* A word of caution: accumulator functions should be efficient *)
let concat l = fold_left (@) [] l

(* For a binary tree, we can define a fold with two accumulators, one for left, one for right sub-trees.
   f combines them into a new accumulator. *)
let rec fold_tree f e t =
    match t with
    | Lf -> e
    | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)

let tree_size t =
    fold_tree (fun _ l r -> 1 + l + r) 0 t

let%test_unit _ =
    let open OUnit2 in
    let open Test_data in
    assert_equal 4 (tree_size tree_basic) ~printer:string_of_int

let tree_sum t =
    fold_tree (fun x l r -> x + l + r) 0 t

let%test_unit _ =
    let open OUnit2 in
    let open Test_data in
    assert_equal 11 (tree_sum tree_basic) ~printer:string_of_int

(* standard tree traversals can be written easily with a list accumulator *)
let tree_preorder t = fold_tree (fun x l r -> [x] @ l @ r) [] t

let%test_unit _ =
    let open OUnit2 in
    let open Test_data in
    assert_equal [1; 0; 6; 4] (tree_preorder tree_basic) ~printer:(print_list ~f:string_of_int)

let tree_inorder t = fold_tree (fun x l r -> l @ [x] @ r) [] t

let%test_unit _ =
    let open OUnit2 in
    let open Test_data in
    assert_equal [0; 1; 4; 6] (tree_inorder tree_basic) ~printer:(print_list ~f:string_of_int)

let tree_postorder t = fold_tree (fun x l r -> l @ r @ [x]) [] t

let%test_unit _ =
    let open OUnit2 in
    let open Test_data in
    assert_equal [0; 4; 6; 1] (tree_postorder tree_basic) ~printer:(print_list ~f:string_of_int)

(* 1. Write a function which, given a list of integers representing expenses, removes them from a budget,
   again represented by an integer. *)
let spend e b = fold_left (-) b e

let%test_unit _ =
    let open OUnit2 in
    assert_equal 0 (spend [0; 0; 0] 0);
    assert_equal 1 (spend [1; 2; 3] 7)

(* 2. Calculate the length of a list using one of the fold functions *)
let length l = fold_left (fun a _ -> a + 1) 0 l

let%test_unit _ =
    let open OUnit2 in
    assert_equal 3 (length [0; 0; 0]);
    assert_equal 0 (length []);
    assert_equal 1 (length ['a'])

(* 3. Use one of the fold functions to find the last element of a list, if any. Behave sensibly if the list is
   empty. *)
let either b a = match a with Some _ -> a | None -> Some b

let last l = 
 fold_right either l None

 let%test_unit _ =
    let open OUnit2 in
    assert_equal None (last []);
    assert_equal (Some 1) (last [1]);
    assert_equal (Some 0) (last [1; 0])

(* 4. Write a function to reverse a list, using one of the fold functions *)
let flip f a b = f b a
let reverse l = fold_left (flip List.cons) [] l

let%test_unit _ =
    let open OUnit2 in
    let open Test_data in
    assert_equal [] (reverse []) ~printer:(print_list ~f:string_of_int);
    assert_equal [0; 1; 2] (reverse [2; 1; 0]) ~printer:(print_list ~f:string_of_int);
    assert_equal [0; 2; 1] (reverse [1; 2; 0]) ~printer:(print_list ~f:string_of_int)

(* 5. Write a version of List.mem using one of the fold functions. Now setify can be defined entirely using folds *)
let mem e l = fold_left (fun a e' -> a || e' == e) false l
let setify l = fold_right (fun e' a -> if mem e' a then a else e' :: a) l

let%test_unit _ =
    let open OUnit2 in
    assert_equal true (mem 1 [1]);
    assert_equal false (mem 0 [1])

(* 6. Use a fold to write a function which, given a list of non-empty strings representing words, returns a
   single string where the words are separated by spaces. Comment on its efficiency. *)
let concat_words l = fold_left (fun (a, sep) w -> (a ^ sep ^ w, " ")) ("", "") l |> fst

let%test_unit _ =
    let open OUnit2 in
    let open Base in
    assert_equal "Hello World" (concat_words ["Hello"; "World"]) ~printer:Fn.id

(* 7. Use fold_tree to write function which calculates the maximum depth of a tree. *)
let max a b = if a >= b then a else b
let max_depth t = fold_tree (fun _ l r -> 1 + max l r) 0 t

let%test_unit _ =
    let open OUnit2 in
    let open Test_data in
    assert_equal 3 (max_depth tree_basic);
    assert_equal 0 (max_depth tree_empty);
    assert_equal 4 (max_depth tree_imbalanced)

(* 8. Compare the time efficiency of one ore more of the functions with the system implementation
   of the same function. (for example our fold-based member function vs List.mem) with regard to both
   computational complexity and actual time taken *)

(* length
   - fold_left length takes linear time O(n) 
   - system: linear time O(n) 
     https://github.com/ocaml/ocaml/blob/6a6f34e48306d573bf3036681cb304ed84415957/stdlib/list.ml#L21-L25 
   - according to benchmarks system is 3 times faster *)
let%test_unit _ =
    let open Test_data in
    let open Benchmark in
    ignore (latency1 ~name:"fold_left length" 1000000L length list_basic);
    ignore (latency1 ~name:"List.length" 1000000L List.length list_basic);

(* 9. Comment on wether the use of folds in each question 1 - 7 is good style *)
(* 9.1. spend - Appropriate use *)
(* 9.2. length - Except for List.length already existing this is appropriate use *)
(* 9.3. last - Inapropriate, incurs complexity penalty of O(n) *)
(* 9.4. reverse - Appropriate due to need for accumulator *)
(* 9.5. mem - List.find might be more appropriate *)
(* 9.6. concat_words - Inapropriate, due to repeated ^ being ineffecient *)
(* 9.7. max_depth - Appropriate *)