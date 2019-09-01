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

let print_int_list = print_list ~f:string_of_int

let times n fn a = 
  let rec times' n l =
    if n = 0 then l
    else times' (n - 1) (fn a :: l)
  in
  times' n []

let compression_input = "((5.000000, 4.583333), (4.500000,5.000000))"
let compression_input_list = [40; 40; 53; 46; 48; 48; 48; 48; 48; 48; 44; 32; 52; 46; 53; 56; 51; 51; 51; 51; 41; 44; 32; 40; 52; 46; 53; 48; 48; 48; 48; 48; 44; 53; 46; 48; 48; 48; 48; 48; 48; 41; 41]
let compression_output_List =  [255; 40; 1; 53; 46; 251; 48; 5; 44; 32; 52; 46; 53; 56; 253; 51; 6; 41; 44; 32; 40; 52; 46; 53; 252; 48; 2; 44; 53; 46; 251; 48; 255; 41; 128]