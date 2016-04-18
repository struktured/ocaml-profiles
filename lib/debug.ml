let debug =
try 
  bool_of_string (Unix.getenv "OCAML_PROFILES_DEBUG")
with _ -> true

let print s = if debug then print_endline @@ Printf.sprintf "[ocaml-profiles]: %s\n" s

