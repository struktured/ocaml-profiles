let ok_or_fail ret = match ret with 
| `Ok o -> o 
| `Error(b,s) -> failwith(string_of_bool b ^ ": " ^ s)
| _ -> failwith("Unexpected variant tag")

let wrap_exn f = try `Ok (f()) with e -> `Error e

let to_tuple = function
  | `Ok _ as o -> o
  | `Error e -> `Error (false, Printexc.to_string e)

let wrap_tuple f = wrap_exn f |> to_tuple

