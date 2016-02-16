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
  Arg.(value & opt string (Opam_config.opam_default()) & info ["o";"opam-target"] ~doc
         ~docv:"OPAM_TARGET")

let compiler_version =
  let default = Opam_config.compiler_version_default () in 
  let doc = "Specifies the ocaml compiler version, defaults to " ^
            default in
  Arg.(value & opt string default & info ["c";"comp"] ~doc
         ~docv:"COMPILER_VERSION")

let list_profiles_flag =
  let doc = "List available profiles to standard output." in
  Arg.(value & flag & info ["list";"l"] ~doc ~docv:"LIST")

open Shell.Infix

let opam_switch ?ssl_no_verify profile compiler_version =
 let f () = begin
    OpamClient.SafeAPI.SWITCH.switch 
      ~compiler:(OpamCompiler.of_string compiler_version)
      ~warning:true
      ~quiet:false
      OpamSwitch.default;
    let eval_cmd = "eval `opam config env`" in Shell.system eval_cmd
  end in
  Env_options.with_env_opts ?ssl_no_verify f
        

module StringSet = CCSet.Make(String)


