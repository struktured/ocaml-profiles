module O = OpamClient.SafeAPI
module Shell = Shell_support.Shell
open Ocaml_profiles_constants
let print = Debug.print

let package_config_file profile = FilePath.concat (Profiles.profile_dir profile)
    package_file_name

let packages profile =
  try 
    package_config_file profile |>
    Shell.lines_of_file |> List.map (fun s -> Re.split (Re_posix.compile_pat " ") s)
                                              |> List.flatten |>
   CCList.filter_map (fun s -> match String.trim s with "" -> None | s -> Some s)
  with _ -> []

let remove_packages ?ssl_no_verify ?(force=true) profile =
  let packages = packages profile in
  match packages with
  | [] -> `Ok ("No packages to install for " ^ profile) | _ ->
  let install_cmd = ssl_no_verify_str ssl_no_verify ^ " opam remove -y " ^ (String.concat " " packages) in
  let ret = Sys.command install_cmd in
  if ret != 0 then `Error (false, Printf.sprintf "%s: nonzero exit status: %d"
                             install_cmd ret) else
  `Ok "Done installing packages"


let install_packages ?ssl_no_verify profile =
  let packages = packages profile in
  match packages with
  | [] -> `Ok ("No packages to install for " ^ profile) | _ ->
  let install_cmd = ssl_no_verify_str ssl_no_verify ^ " opam install -y " ^ (String.concat " " packages) in
  let ret = Sys.command install_cmd in
  if ret != 0 then `Error (false, Printf.sprintf "%s: nonzero exit status: %d"
                             install_cmd ret) else
  `Ok "Done installing packages"


