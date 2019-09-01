(* Chapter 6 - Compressing Data. Page 35 - 49 *)
(* open Chapter_04 - Contains Input and Output modules *)

module Study = struct
  let string_of_int_list (l: int list): string =
    let open Base in
    let init = Bytes.create (List.length l) in
    List.foldi l ~init ~f:(fun n b x -> Bytes.set b n (Char.of_int_exn x); b)
    |> Bytes.to_string

  let%test_unit _ =
    let open OUnit2 in
    let open Base in
    assert_equal Test_data.compression_input (string_of_int_list Test_data.compression_input_list) ~printer:Fn.id

  let int_list_of_string (s: string): int list =
    let open Base in
    String.to_list s |> List.map ~f:Char.to_int

  let%test_unit _ =
    let open OUnit2 in
    assert_equal Test_data.compression_input_list (int_list_of_string Test_data.compression_input) ~printer:Test_data.print_int_list
end