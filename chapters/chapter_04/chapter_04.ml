(* Chapter 4 - Generalized Input. Page 21 - 25 *)

module Input = struct
  (* It is often useful to have a [...] general abstraction, giving
     input and output types which work with OCaml channels and strings.

     [...] fundamental operations on an input will be:
     - finding the current position
     - setting the current position
     - reading a character and advancing the position
     - finding the length of an input

     We need a way to group them togehther, [...] a record is ideal: *)
  type t = {
    pos_in: unit -> int;
    seek_in: int -> unit;
    input_char: unit -> char;
    in_channel_length: int
  }

  (* Now we can build an input from an OCaml in_channel easily *)
  let of_channel (ch: in_channel): t = {
    pos_in = (fun () -> pos_in ch);
    seek_in = seek_in ch;
    input_char = (fun () -> input_char ch);
    in_channel_length = in_channel_length ch
  }

  (* Let us assure ourselves that this structure also works fro abstracting over strings *)
  let of_string (s: string): t =
    let pos = ref 0 in
    {
      pos_in = (fun () -> !pos);
      seek_in = (fun p -> 
          if p < 0 then
            raise (Invalid_argument "seek_in before beginning");
          pos := p
        );
      input_char = (fun () -> 
          if !pos > String.length s - 1 then
            raise End_of_file
          else 
            let c = s.[!pos] in 
            pos := !pos + 1;
            c
        );
      in_channel_length = String.length s
    }

  (* Write a program which can extract the words from a given input, such as a string of a file *)
  let rewind (i: t): unit =
    i.seek_in (i.pos_in () - 1)
end

module Words = struct
  let is_punctuation = function
    | ' ' | '!' | '(' | ')' | '.' | ',' | ';' | ':' -> true
    | _ -> false

  let rec skip_characters (i: Input.t): unit =
    if is_punctuation (i.input_char ()) 
    then skip_characters i
    else Input.rewind i

  let rec collect_characters (b: Buffer.t) (i: Input.t): string =
    (try Some (i.input_char ()) with End_of_file -> None)
    |> function
    | None -> Buffer.contents b
    | Some c -> 
      if is_punctuation c
      then Buffer.contents b
      else (Buffer.add_char b c; collect_characters b i)

  let read_word (i: Input.t): string option =
    try
      skip_characters i;
      Some (collect_characters (Buffer.create 20) i)
    with
      End_of_file -> None

  let read_words (i: Input.t): string list =
    let rec read_words' (a: string list): string list =
      match read_word i with
      | None -> List.rev (List.map String.lowercase_ascii a)
      | Some w -> read_words' (w :: a)
    in
    read_words' []
end


module Output = struct
  (* For a generic  [...] we must have an output_char function to write a single character,
     at least. It is also useful to have an out_channel_length function so we know how many characters
     have been written. *)
  type t = {
    output_char : char -> unit;
    out_channel_length : unit -> int;
  }

  let of_channel (ch: out_channel): t = {
    output_char = (fun c -> output_byte ch (int_of_char c));
    out_channel_length = (fun () -> out_channel_length ch)
  }

  let of_bytes (b: Bytes.t): t = 
    let pos = ref 0 in
    {
      output_char = (fun c -> 
          if !pos < Bytes.length b
          then (Bytes.set b !pos c; pos := !pos + 1)
          else raise End_of_file
        );
      out_channel_length = (fun () -> Bytes.length b)
    }
end

let output_int_list (o: Output.t) (ls: int list): unit = 
  o.output_char '[';
  List.iteri (fun i n -> 
      String.iter o.output_char (string_of_int n);
      o.output_char ';';
      if List.length ls - 1 <> i then o.output_char ' ';
    ) ls;
  o.output_char ']'

let%test_unit _ =
  let open OUnit2 in
  let open Base in
  let expected = "[1; 2; 3; 4; 5;]" in
  let io = Bytes.create (String.length expected) in
  let output = (Output.of_bytes io) in
  output_int_list output [1; 2; 3; 4; 5];
  assert_equal expected (Bytes.to_string io) ~printer:Fn.id

module Solutions = struct
  (* 1. Write a function to build an input from an array of characters *)
  let input_of_array (a: char array): Input.t =
    let pos = ref 0 in
    {
      pos_in = (fun () -> !pos);
      seek_in = (fun p -> pos := p);
      input_char = (fun () -> 
          if !pos >= Array.length a - 1 then
            raise End_of_file
          else
            let c = a.(!pos) in
            pos := !pos + 1;
            c
        );
      in_channel_length = Array.length a
    }

  let%test_unit _ =
    let open OUnit2 in
    let open Base in
    let input = input_of_array [|'a'; 'b'; 'c'; 'd'; 'e'|] in
    assert_equal (input.pos_in ()) 0 ~printer:Int.to_string;
    assert_equal input.in_channel_length 5 ~printer:Int.to_string;

    input.seek_in 1;
    assert_equal (input.pos_in ()) 1 ~printer:Int.to_string;
    assert_equal (input.input_char ()) 'b' ~printer:Char.escaped;
    assert_equal (input.pos_in ()) 2 ~printer:Int.to_string

  (* 2. Write a function input_string of type input -> int -> string which returns the given
     number of characters from the input as string or fewer if the input has ended *)
  let input_string (i: Input.t) (n: int): string =
    let rec input_string' (b: Bytes.t) (n: int): Bytes.t =
      if n = 0 then b
      else try 
          (Bytes.set b (Bytes.length b - n) (i.input_char ()));
          input_string' b (n - 1)
        with End_of_file -> b
    in
    let result = input_string' (Bytes.create n) n in 
    Input.rewind i;
    result |> Bytes.to_string

  let%test_unit _ =
    let open OUnit2 in
    let open Base in
    let alphabet = "abcdefghijklmnopqrstuvwxyz" in
    let input = Input.of_string alphabet in
    assert_equal "a" (input_string input 1) ~printer:Fn.id;
    assert_equal alphabet (input_string input (String.length alphabet)) ~printer:Fn.id;

    (* 3. Extend the input type to include a function input_char_opt which returns a value of type char option,
       with None signalling end of file. Extend the functions input_of_channel and input_of_string appropriately. *)
  module InputOpt = struct
    type t = {
      pos_in: unit -> int;
      seek_in: int -> unit;
      input_char: unit -> char;
      input_char_opt: unit -> char option;
      in_channel_length: int
    }

    (* Now we can build an input from an OCaml in_channel easily *)
    let of_channel (ch: in_channel): t = {
      pos_in = (fun () -> pos_in ch);
      seek_in = seek_in ch;
      input_char = (fun () -> input_char ch);
      input_char_opt = (fun () -> try Some (input_char ch) with End_of_file -> None);
      in_channel_length = in_channel_length ch
    }

    (* Let us assure ourselves that this structure also works fro abstracting over strings *)
    let of_string (s: string): t =
      let pos = ref 0 in
      let in_char = (fun () -> 
          if !pos > String.length s - 1 then
            raise End_of_file
          else 
            let c = s.[!pos] in 
            pos := !pos + 1;
            c
        ) in
      {
        pos_in = (fun () -> !pos);
        seek_in = (fun p -> 
            if p < 0 then
              raise (Invalid_argument "seek_in before beginning");
            pos := p
          );
        input_char = in_char;
        input_char_opt = (fun () -> try Some (in_char ()) with End_of_file -> None);
        in_channel_length = String.length s
      }

    let%test_unit _ =
      let open OUnit2 in
      let open Test_data in
      let open Base in
      let alphabet = "abcdefghijklmnopqrstuvwxyz" in
      let { input_char_opt; _ } = of_string alphabet in
      assert_equal (Some 'a') (input_char_opt ());
      let last = times (String.length alphabet - 1) input_char_opt () |> List.hd_exn in
      assert_equal (Some 'z') last
  end

  (* 4.Extend the input type with a function input_byte which returns an integer representing the next byte,
     or the special value -1 at the end of a file. Comment on the usefulness of this compared with 
     input_char_opt and input_char *)
  module ByteInput = struct
    type t = {
      pos_in: unit -> int;
      seek_in: int -> unit;
      input_char: unit -> char;
      input_byte: unit -> int;
      in_channel_length: int
    }

    let of_string (s: string): t =
      let pos = ref 0 in
      {
        pos_in = (fun () -> !pos);
        seek_in = (fun p -> 
            if p < 0 then
              raise (Invalid_argument "seek_in before beginning");
            pos := p
          );
        input_char = (fun () -> 
            if !pos > String.length s - 1 then
              raise End_of_file
            else 
              let c = s.[!pos] in 
              pos := !pos + 1;
              c
          );
        input_byte = (fun () -> 
            if !pos > String.length s - 1 then -1
            else
              let n = !pos + 1 in
              pos := n;
              n
          );
        in_channel_length = String.length s
      }

    let%test_unit _ =
      let open OUnit2 in
      let open Test_data in
      let alphabet = "abcdefghijklmnopqrstuvwxyz" in
      let { input_byte; _ } = of_string alphabet in
      let len = String.length alphabet in
      assert_equal (List.init len (fun i -> i + 1)) (times len input_byte () |> List.rev) ~printer:print_int_list
  end

  (* 5. Write an input type which raises End_of_file if it reaches a new line (a '\n' character). Use this
     to build a program which reads a line from standard input. *)
  module LineInput = struct
    type t = {
      pos_in: unit -> int;
      seek_in: int -> unit;
      input_char: unit -> char;
      in_channel_length: int
    }

    let of_channel (ch: in_channel): t = {
      pos_in = (fun () -> pos_in ch);
      seek_in = seek_in ch;
      input_char = (fun () -> 
          let c = input_char ch in
          if c = '\n' then raise End_of_file else c
        );
      in_channel_length = in_channel_length ch
    }

    let of_string (s: string): t =
      let pos = ref 0 in
      {
        pos_in = (fun () -> !pos);
        seek_in = (fun p -> 
            if p < 0 then
              raise (Invalid_argument "seek_in before beginning");
            pos := p
          );
        input_char = (fun () -> 
            if !pos > String.length s - 1 then
              raise End_of_file
            else 
              let c = s.[!pos] in 
              if c = '\n' then raise End_of_file;
              pos := !pos + 1;
              c
          );
        in_channel_length = String.length s
      }

    let readline (i: t): string = 
      let rec readline' (b: Buffer.t): Buffer.t =
        try Buffer.add_char b (i.input_char ()); readline' b 
        with End_of_file -> b
      in
      readline' (Buffer.create 0) |> Buffer.contents

    let%test_unit _ =
      let open OUnit2 in
      let open Base in
      let alphabet = "abc\ndef\nghi\njklmnopqrstuvwxyz" in
      let i = of_string alphabet in
      assert_equal "abc" (readline i) ~printer:Fn.id;
  end

  (* 6. Write a function to build an output from a Buffer.t. Show how this can be used to retrieve a final
     string after output is finished *)
  let output_of_buffer (b: Buffer.t): Output.t = {
    output_char = Buffer.add_char b;
    out_channel_length = (fun () -> Buffer.length b)
  }

  let%test_unit _ =
    let open OUnit2 in
    let open Base in
    let b = Buffer.create 0 in
    let o = output_of_buffer b in
    o.output_char 'a';
    o.output_char 'b';
    o.output_char 'c';
    assert_equal "abc" (Buffer.contents b) ~printer:Fn.id
end