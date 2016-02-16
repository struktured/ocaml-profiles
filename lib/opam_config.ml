open Shell_support
let opam_default () = FilePath.concat (Shell.home()) ".opam"

let compiler_version_default () =
  try 
    Shell.run_exn "opam config var ocaml-version"
    |> String.trim
  with _ -> "4.02.3"
