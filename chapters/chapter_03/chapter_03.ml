(* Chapter 3 - Named Tuples with Records. Page 15 - 19 *)

module Study = struct
    (* Let us define a simple type for cartesian coordinates in two dimensions. *)
    type point = { x : float; y : float }

    let p = { x = 4.0; y = 6.0 }

    (* We can add another field to the record, as a label for the point: *)
    type labeled_point = { x : float; y : float; label : string }

    (* We can parametrize it just like a list or variant data type: *)
    type 'a content_point = { x : float; y : float; label : string; content: 'a }

    (* int list content_point *)
    let p' = {  x = 4.5; y = 6.0; label = "p"; content = [1; 2; 3]}

    let make_point x y label content = { x; y; label; content }

    (* We can use dot notation to extract individual parts, alternatively record syntax in patterns *)
    let string_of_point { x; y; label; _ } = Printf.sprintf " %s { x: %f; y: %f; } " label x y

    (* Where we need to copy a record with just one or more fields changed,we can use the with keyword: *)
    let relabel p label = { p with label }

    (* Here is a function to reflect a point about the line x = y *)
    let mirror p = { p with x = p.y; y = p.x }

    (* Individual fields of a record can be made mutable by use of the mutable keyword *)
    type 'a mutable_content_point = { x: float; y: float; label: string; mutable content: 'a }

    (* In fact, OCaml's reference type is just a record with a single, mutable field: *)
    type 'a ref' = { mutable ref : 'a }

    (* The value of a mutable field can be updated using the <- symbol like this: *)
    let () =
        let p' = { x = 1.0; y = 1.0; label = "mutable"; content = [1] } in
        p'.content <- [1; 2]
end

module Questions = struct
    (* 1. Show how to update a reference without using the := operator *)
    let update (r: 'a ref) (u: 'b): 'b ref =  r.contents <- u; r

    (* 2. Using the function the from the "Time Functions" section of the Unix module,
       write a program, which when run, returns a string containing the time and date,
       for example: "It is 2:45 on Wednesday 8 January 2014". *)
    let rec wday = function
        | 0 -> "Sunday"
        | 1 -> "Monday"
        | 2 -> "Tuesday"
        | 3 -> "Wednesday"
        | 4 -> "Thursday"
        | 5 -> "Friday"
        | 6 -> "Saturday"
        | i -> wday (i mod 6)

    let  rec month = function
        | 0 -> "January"
        | 1 -> "February"
        | 2 -> "March"
        | 3 -> "April"
        | 4 -> "May"
        | 5 -> "June"
        | 6 -> "July"
        | 7 -> "August"
        | 8 -> "September"
        | 9 -> "October"
        | 10 -> "November"
        | 11 -> "December"
        | i -> month (i mod 6)

    let announce input_time = 
        let open Unix in
        let open Base in
        input_time 
        |> Option.value ~default:(time ())
        |> gmtime 
        |> (fun {tm_hour; tm_min; tm_wday; tm_mday; tm_mon; tm_year; _} -> Printf.sprintf "It is %i:%i on %s, %i. %s %i" tm_hour tm_min (wday tm_wday) tm_mday (month tm_mon) (tm_year + 1900))

    let%test_unit _ =
        let open OUnit2 in
        let open Base in
        assert_equal "It is 20:53 on Friday, 16. August 2019" (announce (Some 1565988780.)) ~printer:Fn.id
end

