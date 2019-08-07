open Types

(*          1
 *    +-----+-----+
 *    0           6
 * +--+--+     +--+--+
 * Lf    Lf    4     Lf
 *)
let tree_basic = (Br (1, Br (0, Lf, Lf), Br (6, Br (4, Lf, Lf), Lf)))

let (>>) fa fb a = fa a |> fb

let print_list ~f = 
    let open Base in
    List.map ~f >> String.concat ~sep:"; " >> Printf.sprintf "[%s]"
