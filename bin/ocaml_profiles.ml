open Cmdliner
open Shell_support
open Shell
open Ocaml_profiles_constants
open Arguments
let () =
  ignore(clean_profiles_dir());
  match safe_cmd with `Error _ -> exit 1 | _ -> exit 0
