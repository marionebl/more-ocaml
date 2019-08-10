(* We can make our own data type for OCaml's built-in lists like this *)
type 'a list = Nil | Cons of 'a * 'a list

(* It is also possible to define an infinitely-long list, where elements are only
   produced when we actually need them. This is known as lazy list. Instead of a tail,
   we use a tail function *)
type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

(* Write a function which, given an integer n, builds the lazy list of all integers: *)
let rec lseq n =
    Cons (n, fun () -> lseq (n + 1))

(* Write functions to extract items from the list. Here are lazy head and tail functions: *)
let lhd (Cons (n, _)) = n
let ltail (Cons (_, t)) = t ()

let%test _ = 0 == lhd (lseq 0)
let%test _ = 1 == lhd (lseq 1)
let%test _ = 1 == lhd (ltail (lseq 0))

(* Here are lazy versions of the familiar take and drop functions, which take or drop the first n elements of a list *)
let rec ltake (Cons(h, t)) n =
  if n < 0 then
    raise (Invalid_argument (Printf.sprintf "n must be greater or equal to 0, received %i" n))
  else
    match n with
    | 0 -> []
    | _ -> h :: ltake (t ()) (n - 1)

let%test_unit _ = 
    let open OUnit2 in
    let open Test_data in
    let open Base in
    assert_equal [10; 11; 12; 13; 14] (ltake (lseq 10) 5) ~printer:print_int_list;
    assert_equal (Error (Invalid_argument "n must be greater or equal to 0, received -1")) (Result.try_with (fun _ -> ltake (lseq 0) (-1)))

let rec ldrop (Cons(_, t)) n =
    match n with
    | 0 -> t ()
    | _ -> ldrop (t ()) (n - 1)

(* let%test_unit _ = 
    let open OUnit2 in
    let open Test_data in
    assert_equal (lseq 15) (ldrop (lseq 10) 5); *)