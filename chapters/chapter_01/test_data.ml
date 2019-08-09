open Types

let list_basic = List.init 1001 Base.Fn.id 

let tree_empty = Lf

(*          1
 *    +-----+-----+
 *    0           6
 * +--+--+     +--+--+
 * Lf    Lf    4     Lf
 *)
let tree_basic = (Br (1, Br (0, Lf, Lf), Br (6, Br (4, Lf, Lf), Lf)))


(*          1
 *    +-----+-----+
 *   Lf           6
 *             +--+--+
 *             4     Lf
 *          +--+--+
 *          5     Lf
 *       +--+--+
 *       Lf    Lf
 *)
let tree_imbalanced = (Br (1, Lf, Br (6, Br (4, Br (5, Lf, Lf), Lf), Lf)))

let (>>) fa fb a = fa a |> fb

let print_list ~f = 
    let open Base in
    List.map ~f >> String.concat ~sep:"; " >> Printf.sprintf "[%s]"
