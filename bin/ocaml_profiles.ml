open Cmdliner
open Shell_support
open Shell

let profiles_default_url () =
  try
    Unix.getenv "OCAML_PROFILES_URL"
 with Not_found -> "https://github.com/struktured/ocaml-profiles"

let profiles_repo_url =
  let doc = "Specifies a ocam profile repository to fetch profiles from. " ^
            "The OCAML_PROFILES_URL environment variable overrides the default value." in
  Arg.(value & (opt string) (profiles_default_url()) & info ["r";"repo"] ~doc ~docv:"REPO_URL")

let ssl_no_verify =
  let doc = "Use to disable verification for environments with broken certificates." in
  Arg.(value & flag & info ["ssl-no-verify"] ~doc ~docv:"SSL_NO_VERIFY")

let opam_default () = FilePath.concat (home()) ".opam"
let pinned_file_name = "pinned"
let package_file_name = "packages"
let compiler_version_default = "4.02.1"
let ssl_no_verify_env = "GIT_SSL_NO_VERIFY"

let ssl_no_verify_str = function
  | None -> ""
  | Some b -> ssl_no_verify_env ^ "=" ^ string_of_bool b ^ " "

let profiles_dir = ".ocaml-profiles"
let profiles_file_name = "profiles"

let profile_dir profile = FilePath.concat profiles_dir profile

let pinned_config_file profile = FilePath.concat (profile_dir profile)
    pinned_file_name

let profiles_config_file profile = FilePath.concat (profile_dir profile)
    profiles_file_name

let pins profile =
  let file = pinned_config_file profile in open_in file |>
  Std.input_list

let profiles profile =
  let file = profiles_config_file profile in open_in file |>
  Std.input_list

let package_config_file profile = FilePath.concat (profile_dir profile)
    package_file_name

let packages profile =
  let file = package_config_file profile in open_in file |>
  Std.input_list |> List.map (fun s -> Re.split (Re_posix.compile_pat " ") s) |> List.flatten

let pinned_config_file_target opam_repo_target compiler_version
  = FilePath.concat opam_repo_target @@
    FilePath.concat compiler_version pinned_file_name

module Kind = struct
  type t = [`Git | `Path | `Hg | `Darcs]
  let to_string = function `Git -> "git" | `Path -> "path" | `Hg -> "hg"
   | `Darcs -> "darcs"
  let of_string s = match String.lowercase s with
   | "path" -> `Path
   | "git" -> `Git
   | "hg" -> `Hg
   | "darcs" -> `Darcs
   | k -> failwith("unknown kind: " ^ k)
end

type pin_entry = {name:string;kind:Kind.t; target:string}

let pins profile =
  let file = pinned_config_file profile in open_in file |>
  Std.input_list |>
  List.map String.trim |>
  List.filter (fun s -> String.length s > 0) |>
  List.map (fun s -> Re.split (Re_posix.compile_pat " ") s) |>
  List.map (function [name;kind;target] ->
             {name;kind=Kind.of_string kind;target} | 
               l -> failwith("unxpected number of columns for line: " ^
                                 String.concat " " l))
let read_all (dir:string) = 
  let dir = Unix.opendir dir in
  let rec iter l = try 
      let entry = Unix.readdir dir in 
      if String.contains_from entry 0 '.' then iter l else
      entry::(iter l) with End_of_file -> l in
  iter []

(* TODO not implemented *)
let get_profiles () = ["None"]

let profile =
  let doc = "Specifies a profile to apply to the repository. "
            (* ^ "Possible choices are: \n[" ^ 
            (String.concat ", " @@ get_profiles ()) ^ "]." *) in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PROFILE")

let opam_repo_target =
  let doc = "Specifies the target opam repository, typically ~/.opam" in
  Arg.(value & opt string (opam_default()) & info ["o";"opam-target"] ~doc
         ~docv:"OPAM_TARGET")

let compiler_version =
  let doc = "Specifies the ocaml compiler version, defaults to " ^
            compiler_version_default in
  Arg.(value & opt string compiler_version_default & info ["comp";"c"] ~doc
         ~docv:"COMPILER_VERSION")

open Shell.Infix

let print s = print_endline @@ Printf.sprintf "[ocaml-profiles]: %s\n" s

let checkout_profile ~ssl_no_verify profile url =
  let profile_dir = profile_dir profile in
  FileUtil.rm ~recurse:true ~force:FileUtil.Force [profiles_dir];
  let ssl_no_verify = match ssl_no_verify with
  | true -> Some true
  | false -> None in
  match Git.clone ?ssl_no_verify
    ~single_branch:true ~target:profile_dir ~branch_or_tag:profile url with
  | `Error _ -> Git.clone
      ~target:profile_dir ~branch_or_tag:("profiles/" ^ profile) url
  | `Ok _ as o -> o

let add_pins profile =
  let pins = pins profile in
  let remove_pin {name;kind;target} =
      Shell.system @@ Printf.sprintf "opam pin -n -y remove %s" name in
  let add_pin {name;kind;target} =
      Shell.system @@ Printf.sprintf "opam pin -y add -k %s %s %s"
        (Kind.to_string kind) name target in
  let remove_add p = remove_pin p >>= fun s -> print s; add_pin p >>=
    fun s -> print s; `Ok ("added and removed " ^ p.name) in
  CCList.fold_while (fun res pin ->
      match res with 
       | `Error _ as e -> e, `Stop
       | `Ok _ -> (remove_add pin), `Continue) (`Ok "apply_pins: start") pins

type profile = {name:string;url:string}

let load_profiles profile url  =
  let profiles = profiles profile in
  let splitter = Re_posix.compile_pat " " |> fun re -> Re.split re in
  CCList.map splitter profiles |> CCList.filter_map
  (function 
    | [profile'] -> Some {name=profile'; url}
    | profile'::(url'::_) -> Some {name=profile';url=url'}
    | _ -> None) 

let opam_switch ?ssl_no_verify profile compiler_version =
  let switch_cmd = ssl_no_verify_str ssl_no_verify ^ " opam switch " ^ compiler_version in
  Shell.system switch_cmd >>= fun _ ->
  let eval_cmd = "eval `opam config env`" in
  Shell.system eval_cmd

let install_packages ?ssl_no_verify profile =
  let packages = packages profile in
  let install_cmd = ssl_no_verify_str ssl_no_verify ^ " opam reinstall -y " ^ (String.concat " " packages) in
  let ret = Sys.command install_cmd in
  if ret != 0 then `Error (false, Printf.sprintf "%s: nonzero exit status: %d"
                             install_cmd ret) else
  `Ok "Done installing packages"

module StringSet = CCSet.Make(String)

let ok_or_fail ret = match ret with 
| `Ok o -> o 
| `Error(b,s) -> failwith(string_of_bool b ^ ": " ^ s)
| _ -> failwith("Unexpected variant tag")


let clean_profiles_dir () =
  `Ok (FileUtil.rm ~recurse:true ~force:FileUtil.Force [profiles_dir])

let rec run profile opam_repo_target compiler_version profiles_url ssl_no_verify =
 let add_profiles added_profiles =
  let profiles = load_profiles profile profiles_url in
  CCList.fold_while 
    (fun set (profile:profile) ->
      match StringSet.exists ((=) profile.name) added_profiles with
      | true -> print @@ "WARNING: Skipping profile \"" ^ profile.name ^ "to avoid cycling!";
       `Ok added_profiles, `Continue
      | false -> begin
      let set = StringSet.add profile.name added_profiles in
      match run profile.name opam_repo_target compiler_version profile.url ssl_no_verify
      with 
      | `Ok _  -> `Ok set, `Continue
      |  e -> e, `Stop end) (`Ok added_profiles) profiles in

  print @@ Printf.sprintf "\"%s\" to opam repository \"%s\" with
  compiler version %s...\n" profile opam_repo_target compiler_version;
  ignore(ok_or_fail @@ opam_switch ~ssl_no_verify profile compiler_version);
  ignore(ok_or_fail @@ checkout_profile ~ssl_no_verify profile profiles_url);
  let set = ok_or_fail @@ add_profiles StringSet.empty in
  ignore(ok_or_fail @@ add_pins profile);
  ignore(ok_or_fail @@ install_packages ~ssl_no_verify profile);`Ok set

let cmd =
  let doc = "Apply an ocaml profile to a target opam repository" in
  Term.(ret (pure run $ profile $ opam_repo_target $ compiler_version $ profiles_repo_url $ ssl_no_verify)),
  Term.info "ocaml-profiles" ~version:"1.0" ~doc

let safe_cmd =
  try
    ignore(Term.eval cmd);
    clean_profiles_dir()
  with e -> ignore(clean_profiles_dir()); raise e

let () =
  match safe_cmd with `Error _ -> exit 1 | _ -> exit 0