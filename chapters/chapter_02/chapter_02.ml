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

let%test_unit _ = 
    let open OUnit2 in
    assert_equal 16 (ldrop (lseq 10) 5 |> lhd) ~printer:string_of_int

(* map and filter have easy analogoues in the lazy world *)
let rec lmap f (Cons (h, tf)) = Cons (f h, fun () -> (lmap f (tf ())))

let rec lfilter f (Cons (h, tf)) =
  if f h then
    Cons (h, fun () -> lfilter f (tf ()))
  else
    lfilter f (tf ())

let cubes = 
  lseq 1 
  |> lmap (fun x -> x * x * x) 
  |> lfilter (fun x -> x mod 5 = 0)

let primes =
  let rec mkprimes (Cons (h, tf)) =
    Cons (h, fun () -> mkprimes (lfilter (fun x -> x mod h <> 0) (tf ())))
  in
  lseq 2 |> mkprimes

(* There is an analogue to apped. We can combine two lists fairly, taking elements in turn from each. *)
let rec interleave (Cons (h, tf)) l =
  Cons (h, fun () -> interleave l (tf ()))

(* The list alternating between zeros and ones can be built with interleave and a function to build constant lists *)
let rec lconst c =
  Cons (c, fun () -> lconst c)

let zeros_ones = interleave (lconst 0) (lconst 1)

(* calculate the lazy list of all ordinary lists of zeros and ones. Prepend a zero and a one to the list, interleaving the resulting lists *)
let rec allfrom l =
  Cons (l, fun () -> interleave (allfrom (0 :: l)) (allfrom (1 :: l)))

(* 1. Write the lazy list whose elements are the numbers of 1, 2, 4, 8, 16 ... *)
let rec mk_series n f = Cons (n, fun () -> mk_series (n * f) f)
let quad_series = mk_series 1 2

(* 2. Write a function to return the nth element of a lazy list where element zero is the head of the list *)
let rec lnth (Cons (h, tf)) n = 
  if n = 0 then h else lnth (tf ()) (n - 1)

let%test_unit _ = 
  let open OUnit2 in
  assert_equal 10 (lnth (lseq 0) 10);
  assert_equal 11 (lnth (lseq 1) 10);
  assert_equal 2 (lnth (lseq 1) 1)

(* 3. Write a function which, given a list, returns the lazy list forming a repeaded sequence taken from that list.
   For example given the list [1; 2; 3] it should return a lazy list with elements 1, 2, 3, 1, 2, 3, 1, 2, ... *)
let to_ring l =
  let len = List.length l in
  let rec to_ring' l i = Cons (List.nth l i, fun () -> to_ring' l ((i + 1) mod len)) in
  to_ring' l 0

let%test_unit _ =
  let open OUnit2 in
  let open Test_data in
  assert_equal [1; 2; 3] (ltake (to_ring [1; 2; 3]) 3) ~printer:(print_list ~f:string_of_int);
  assert_equal [1; 2; 3; 1; 2; 3; 1] (ltake (to_ring [1; 2; 3]) 7) ~printer:(print_list ~f:string_of_int);
  assert_equal [1; 2; 3; 4; 1; 2; 3] (ltake (to_ring [1; 2; 3; 4]) 7) ~printer:(print_list ~f:string_of_int)

(* 4. Write a lazy list whose elements are the fibonacci numbers 0, 1, 1, 2, 3, 5, 8 ... whose first two
   elements are zero and one by definition, and each ensuing element is the sum of the previous two. *) 
let fibonacci _ =
  let rec fibonacci' m n = Cons (m + n, fun () -> fibonacci' (if m + n = 0 then 1 else n) (m + n)) in
  fibonacci' 0 0

(* 5. Write the function unleave which, given a lazy list, returns two lazy lists, one containing elements
   at positions 0, 2, 4, 6 ... of the original list, and the other containing elements at positions 1, 3, 5, 7 ... *)
let lfilteri f l = 
  let rec lfilteri' f (Cons (h, tf)) i =
    if f i h then
      Cons (h, fun () -> lfilteri' f (tf ()) (i + 1))
    else
      lfilteri' f (tf ()) (i + 1)
  in
  lfilteri' f l 0

let unleave l = (lfilteri (fun i _ -> i mod 2 = 0) l), (lfilteri (fun i _ -> i mod 2 <> 0) l)

let%test_unit _ =
  let open Base in
  let open OUnit2 in
  let open Test_data in
  assert_equal [0; 2] (unleave (lseq 0) |> fst |> Fn.flip ltake 2) ~printer:(print_list ~f:Int.to_string);
  assert_equal [1; 3] (unleave (lseq 0) |> snd |> Fn.flip ltake 2) ~printer:(print_list ~f:Int.to_string)

(* 6. Alphanumeric labels in documents go A, B, C ... X, Y, Z, AA, AB, ... BA, BB ... AAA ... Write
   the lazy list containing strings representing this sequence. You may (mis)zse the Standard Library
   function Char.escaped to convert a character to a string *)
let rec label n = 
  if n <= 25 then 
    Char.chr (n + 65) |> Char.escaped
  else 
    label ((n / 26) - 1) ^ label (n mod 26)

let alpha_labels = lseq 0 |> lmap label

let%test_unit _ =
  let open OUnit2 in
  let open Base in
  assert_equal "A" (lnth alpha_labels 0) ~printer:Fn.id;
  assert_equal "B" (lnth alpha_labels 1) ~printer:Fn.id;
  assert_equal "AA" (lnth alpha_labels 26) ~printer:Fn.id;
  assert_equal "DCS" (lnth alpha_labels 2800) ~printer:Fn.id;