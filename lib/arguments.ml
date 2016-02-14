open Cmdliner
open Shell_support
open Shell
open Ocaml_profiles_constants
open Debug
open Profiles
let profiles_repo_url =
  let doc = "Specifies a ocam profile repository to fetch profiles from. " ^
            "The OCAML_PROFILES_URL environment variable overrides the default value." in
  Arg.(value & (opt string) (profiles_default_url()) & info ["r";"repo"] ~doc ~docv:"REPO_URL")

let profiles_repo_url =
  let doc = "Specifies a ocam profile repository to fetch profiles from. " ^
            "The OCAML_PROFILES_URL environment variable overrides the default value." in
  Arg.(value & (opt string) (profiles_default_url()) & info ["r";"repo"] ~doc ~docv:"REPO_URL")

let follow_profiles =
  let doc = "Specifies whether to follow profiles recursively. " in
  Arg.(value & flag & info ["f";"follow"] ~doc ~docv:"FOLLOW")

let ssl_no_verify =
  let doc = "Use to disable verification for environments with broken certificates." in
  Arg.(value & flag & info ["ssl-no-verify"] ~doc ~docv:"SSL_NO_VERIFY")

let opam_default () = FilePath.concat (home()) ".opam"

let compiler_version_default () =
  try 
    Shell.run_exn "opam config var ocaml-version"
    |> String.trim
  with _ -> "4.02.3"
let ssl_no_verify_env = "GIT_SSL_NO_VERIFY"
let ssl_no_verify_str = function
  | None -> ""
  | Some b -> ssl_no_verify_env ^ "=" ^ string_of_bool b ^ " "
let package_config_file profile = FilePath.concat (profile_dir profile)
    package_file_name

let packages profile =
  try 
    package_config_file profile |>
    Shell.lines_of_file |> List.map (fun s -> Re.split (Re_posix.compile_pat " ") s)
                                              |> List.flatten |>
   CCList.filter_map (fun s -> match String.trim s with "" -> None | s -> Some s)
  with _ -> []

let depext_config_file profile = FilePath.concat (profile_dir profile)
    depext_file_name

let depexts profile =
  try
    depext_config_file profile |>
    Shell.lines_of_file |> List.map (fun s -> Re.split (Re_posix.compile_pat " ") s)
                                              |> List.flatten |>
   CCList.filter_map (fun s -> match String.trim s with "" -> None | s -> Some s)
  with _ -> []


let pinned_config_file_target opam_repo_target compiler_version
  = FilePath.concat opam_repo_target @@
    FilePath.concat compiler_version pinned_file_name

let read_all (dir:string) = 
  let dir = Unix.opendir dir in
  let rec iter l = try 
      let entry = Unix.readdir dir in 
      if String.contains_from entry 0 '.' then iter l else
      entry::(iter l) with End_of_file -> l in
  iter []

module Operation = 
  struct 
    type t = List | Show of string | Apply of string

    let of_string ?(list_profiles_flag=false) first second =
        if list_profiles_flag then List else
        match String.lowercase first with
        | "list" -> List
        | "show" -> Show second
        | "apply" -> Apply second
        | s -> Apply s

    let to_string t =
      match t with
       | List -> "list"
       | Show s -> "show " ^ s
       | Apply s -> "apply " ^ s
  end

let operation =
  let doc = "Specifies a profile to apply to the repository, or an operation of type 'list', 'show', or 'apply'." in
  Arg.(value & pos 0 (string) "list" & info [] ~doc ~docv:"PROFILE|list|show|apply")

let profile =
  let doc = "The profile argument for an operation preceding it." in
  Arg.(value & pos 1 (string) "" & info [] ~doc ~docv:"PROFILE")

let opam_repo_target =
  let doc = "Specifies the target opam repository, typically ~/.opam" in
  Arg.(value & opt string (opam_default()) & info ["o";"opam-target"] ~doc
         ~docv:"OPAM_TARGET")

let compiler_version =
  let default = compiler_version_default () in 
  let doc = "Specifies the ocaml compiler version, defaults to " ^
            default in
  Arg.(value & opt string default & info ["c";"comp"] ~doc
         ~docv:"COMPILER_VERSION")

let list_profiles_flag =
  let doc = "List available profiles to standard output." in
  Arg.(value & flag & info ["list";"l"] ~doc ~docv:"LIST")

open Shell.Infix

let opam_switch ?ssl_no_verify profile compiler_version =
  let switch_cmd = ssl_no_verify_str ssl_no_verify ^ " opam switch " ^ compiler_version in
  Shell.system switch_cmd >>= fun _ ->
  let eval_cmd = "eval `opam config env`" in
  Shell.system eval_cmd

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

let install_depexts ?ssl_no_verify profile =
  let depexts = depexts profile in
  match depexts with
  | [] -> `Ok ("No dependency extensions to install for " ^ profile) | _ ->
  let install_cmd = ssl_no_verify_str ssl_no_verify ^ " opam depext " ^ (String.concat " " depexts) in
  let ret = Sys.command install_cmd in
  if ret != 0 then `Error (false, Printf.sprintf "%s: nonzero exit status: %d"
                             install_cmd ret) else
  `Ok "Done installing dependency extensions"


module StringSet = CCSet.Make(String)

let ok_or_fail ret = match ret with 
| `Ok o -> o 
| `Error(b,s) -> failwith(string_of_bool b ^ ": " ^ s)
| _ -> failwith("Unexpected variant tag")


