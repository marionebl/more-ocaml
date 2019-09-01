(* Chapter 5 - Streams of Bits. Page 27 - 34 *)
type input = {
  pos_in: unit -> int;
  seek_in: int -> unit;
  input_char: unit -> char;
  in_channel_length: int
}

(* Let us build a bit stream based in our input type *)
type input_bits = {
  input: input;
  mutable byte: int;
  mutable bit: int;
}

(* We can define a function to build and input_bits from an input *)
let input_bits_of_input (input: input): input_bits = {
  input;
  byte = 0;
  bit = 0;
}

(* The function to get the next bit is simple. If bit is zero, we must load a new byte from the input and
   return the next bit. If bit is non-zero, we extract the given bit, and halve bit ready for next time. *)
let rec getbit (b: input_bits): bool = 
  if b.bit = 0 then
    begin
      b.byte <- int_of_char (b.input.input_char ());
      b.bit <- 128;
      getbit b
    end
  else 
    let r = b.byte land b.bit > 0 in
    b.bit <- b.bit / 2;
    r

(* We can align the bit stream on the next byte boundary trivially. *)
let align (b: input_bits): unit = 
  b.bit <- 0

(* We can write a function getval to return a given number of bits considered as an integer, allowing us to
   read a data field of any width. *)
let getval (b: input_bits) (n: int): int =
  if n <= 0 || n > 31 then
    raise (Invalid_argument "getval")
  else 
    let r = ref 0 in
    for x = n - 1 downto 0 do
      r := !r lor ((if getbit b then 1 else 0) lsl x)
    done; 
    !r

(* Whe shall use the following 20 byte TCP datagram header as an example *)
let tcp_header = "00 26 bb 14 62 b7 cc 33 58 55 1e ed 08 00 45 00 03 78 f7 ac" 

let input_of_string (s: string): input =
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

let print_header (h: string): unit = 
  let i = input_of_string h in
  let b = input_bits_of_input i in
  let src_port = getval b 16 in
  let dest_port = getval b 16 in
  let seq_number = getval b 32 in
  let ack_number = getval b 32 in
  let _ = getval b 4 in (* data offset *)
  let _ = getval b 6 in (* reserved *)
  let urgent = getbit b in
  let ack = getbit b in
  let push = getbit b in
  let reset = getbit b in
  let syn = getbit b in
  let fin = getbit b in
  let receive = getval b 16 in
  let checksum = getval b 16 in
  let urgent_pointer = getval b 16 in
  let p = Printf.printf in
    p "Source port = %i\n" src_port;
    p "Destination = %i\n" dest_port;
    p "Sequence = %i\n" seq_number;
    p "Acknowledgement Number = %i\n" ack_number;
    p "\nFlags:\n";
    p "Urgent = %b\n" urgent;
    p "Ack = %b\n" ack;
    p "Push = %b\n" push;
    p "Reset = %b\n" reset;
    p "Syn = %b\n" syn;
    p "Fin = %b\n" fin;
    p "Receive window size = %i\n" receive;
    p "Checksum = %i\n" checksum;
    p "Urgent pointer = %i\n" urgent_pointer

type output = {
  output_char : char -> unit;
  out_channel_length : unit -> int;
}

let output_of_bytes (b: Bytes.t): output = 
  let pos = ref 0 in
  {
    output_char = (fun c -> 
        if !pos < Bytes.length b
        then (Bytes.set b !pos c; pos := !pos + 1)
        else raise End_of_file
      );
    out_channel_length = (fun () -> Bytes.length b)
  }

(* The type for output bit streams is rather similiar to that for input bit streams. *)
type output_bits = {
  output : output;
  mutable obyte : int;
  mutable obit : int
}

let output_bits_of_output (output: output): output_bits = {
  output;
  obyte = 0;
  obit = 7
}

let flush (o: output_bits) =
  if o.obit < 7 then o.output.output_char (char_of_int o.obyte);
  o.obyte <- 0;
  o.obit <- 7

let rec putbit (o: output_bits) (b: int) =
  if o.obit = (-1) then 
    begin
      flush o;
      putbit o b 
    end
  else 
    begin
      if b <> 0 then o.obyte <- o.obyte lor (1 lsl o.obit);
      o.obit <- o.obit - 1
    end

let putval (o: output_bits) (v: int) (l: int): unit =
  for x = l - 1 downto 0 do
    putbit o (v land (1 lsl x))
  done

(* 1. Spezialize the function getval so that writing 8 bits at a time when the input is aligned is optimized.
   Benchmark this function against the naive one. *)
let getval_fast (i: input_bits) (n: int): int =
  if i.bit = 0 && n = 8 
  then i.input.input_char () |> int_of_char
  else getval i n

let%test_unit _ =
  let open OUnit2 in
  let open Benchmark in
  let i = input_of_string tcp_header in
  let b = input_bits_of_input i in
  let c = fun () -> tcp_header |> input_of_string |> input_bits_of_input in
  assert_equal (getval_fast b 8) (getval b 8) ~printer:string_of_int;
  ignore (latency1 ~name:"getval" 1000000000L getval (c ()));
  ignore (latency1 ~name:"getval_fast" 1000000000L getval_fast (c ()))