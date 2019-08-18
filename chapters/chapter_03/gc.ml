open Base
open Chapter_03

let () =
    match Sys.argv |> Array.to_list with
    | _ :: "summary" :: _ -> Questions.gc_summary ()
    | _ :: "verbosity" :: flag :: _ -> Questions.gc_verbosity flag
    | _ :: cmd :: _ -> failwith (Printf.sprintf "Unknown command %s" cmd)
    | _ -> failwith "Missing command"