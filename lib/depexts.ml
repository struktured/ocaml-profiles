open Ocaml_profiles_constants 
open Shell_support
let depext_config_file profile = FilePath.concat (Profiles.profile_dir profile)
    depext_file_name

let depexts profile =
  try
    depext_config_file profile |>
    Shell.lines_of_file |> List.map (fun s -> Re.split (Re_posix.compile_pat " ") s)
                                              |> List.flatten |>
   CCList.filter_map (fun s -> match String.trim s with "" -> None | s -> Some s)
  with _ -> []

let apply ?ssl_no_verify profile =
  let depexts = depexts profile in
  match depexts with
  | [] -> `Ok ("No dependency extensions to install for " ^ profile) | _ ->
  let f () =
  let install_cmd = "opam depext " ^ (String.concat " " depexts) in
  let ret = Sys.command install_cmd in
  if ret != 0 then `Error (false, Printf.sprintf "%s: nonzero exit status: %d"
                             install_cmd ret) else
  `Ok "Done installing dependency extensions" in
  Env_options.with_env_opts ?ssl_no_verify f
 
