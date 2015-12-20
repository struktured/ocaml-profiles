open Cmdliner
open Shell_support
open Shell
let print s = print_endline @@ Printf.sprintf "[ocaml-profiles]: %s\n" s

let profiles_default_url () =
  try
    Unix.getenv "OCAML_PROFILES_URL"
 with Not_found -> "https://github.com/struktured/ocaml-profiles"

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
let pinned_file_name = "pinned"
let package_file_name = "packages"
let depext_file_name = "depext"
let compiler_version_default () =
  try 
    Shell.run_exn "opam config var ocaml-version"
    |> String.trim
  with _ -> "4.02.3"
let ssl_no_verify_env = "GIT_SSL_NO_VERIFY"

let ssl_no_verify_str = function
  | None -> ""
  | Some b -> ssl_no_verify_env ^ "=" ^ string_of_bool b ^ " "

let profiles_dir = ".ocaml-profiles"
let profiles_file_name = "profiles"

let profile_dir profile = 
  if FileUtil.test FileUtil.Is_dir profile then
    profile 
  else
    FilePath.concat profiles_dir profile

let pinned_config_file profile = 
    FilePath.concat 
        (profile_dir profile)
        pinned_file_name

let profiles_config_file profile = FilePath.concat (profile_dir profile)
    profiles_file_name
let profiles profile =
  try
    profiles_config_file profile |>
    Shell.lines_of_file
  with _ -> []

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

module Pin_entry = struct
  type t = {name:string;kind:Kind.t; target:string}
  let to_string t = t.name ^ " " ^ 
    Kind.to_string t.kind ^ " " ^ t.target
end

let _pins profile =
  let open Pin_entry in
  pinned_config_file profile |>
  Shell.lines_of_file |>
  List.map String.trim |>
  List.filter (fun s -> String.length s > 0) |>
  List.map (fun s -> Re.split (Re_posix.compile_pat " ") s) |>
  List.map (function [name;kind;target] ->
             {name;kind=Kind.of_string kind;target} |
               l -> failwith("unxpected number of columns for line: " ^
                                 String.concat " " l))

let pins profile =
  try
    print @@ "pins: for profile " ^ profile;
    _pins profile
(*    pinned_config_file profile |> Shell.lines_of_file *)
  with e ->
    print ("pins: error: " ^ (Printexc.to_string e)); 
    []


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

let profiles_branch_prefix = "profiles"

let checkout_profile ~ssl_no_verify profile url =
  if FileUtil.test FileUtil.Is_dir profile then begin
    print @@ "checkout_profile: Profile " ^ profile ^ " is a local directory";
    `Ok profile 
  end
  else
  let profile_dir = profile_dir profile in
  let ssl_no_verify = match ssl_no_verify with
  | true -> Some true
  | false -> None in
  let qualified_profile = profiles_branch_prefix ^ "/" ^ profile in
  Git.clone ?ssl_no_verify
    ~single_branch:true ~target:profile_dir ~branch_or_tag:qualified_profile url

let add_pins profile =
  print @@ "add_pins: " ^ profile;
  let open Pin_entry in
  let pins = pins profile in
  let remove_pin {name;kind;target} =
      Shell.system @@ Printf.sprintf "opam pin -n -y remove %s" name in
  let add_pin {name;kind;target} =
      Shell.system @@ Printf.sprintf "opam pin -n -y add -k %s %s %s"
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

let list_profiles url =
   let open Git.Response.Remote_ref in
   Git.ls_remote url |> function |`Error _ as e -> e | `Ok r ->
     print_endline @@ "Available profiles in \"" ^ url ^ "\":";
     CCList.filter_map (fun r -> match r.ref_type with 
      | `Branch | `Tag -> 
          let value = match r.value with
          | s when CCString.find ~sub:"profiles/" s = 0 -> Some 
            (String.sub s (String.length "profiles/") (String.length s - String.length "profiles/"))
          | _ -> None in
          CCOpt.map (fun v -> "\t" ^ v) value | _ -> None) r |> fun filtered ->
     List.iter print_endline filtered; `Ok filtered

let opam_switch ?ssl_no_verify profile compiler_version =
  let switch_cmd = ssl_no_verify_str ssl_no_verify ^ " opam switch " ^ compiler_version in
  Shell.system switch_cmd >>= fun _ ->
  let eval_cmd = "eval `opam config env`" in
  Shell.system eval_cmd

let install_packages ?ssl_no_verify profile =
  let packages = packages profile in
  match packages with 
  | [] -> `Ok ("No packages to install for " ^ profile) | _ ->
  let install_cmd = ssl_no_verify_str ssl_no_verify ^ " opam reinstall -y " ^ (String.concat " " packages) in
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


let clean_profiles_dir () =
  `Ok (FileUtil.rm ~recurse:true ~force:FileUtil.Force [profiles_dir])

let rec _run added_profiles profile opam_repo_target profiles_url ssl_no_verify =
 let add_profiles added_profiles =
  let profiles = load_profiles profile profiles_url in
  CCList.fold_while 
    (fun r profile -> match r with
    | `Ok set -> begin
      match StringSet.exists ((=) profile.name) set with
      | true -> print @@ "WARNING: Skipping profile \"" ^ profile.name ^ " to avoid cycling!";
       `Ok set, `Continue
      | false -> begin
        let set = StringSet.add profile.name set in
        match _run set profile.name opam_repo_target profile.url ssl_no_verify
      with 
      | `Ok _  -> `Ok set, `Continue
      | e -> e, `Stop end end
    | e -> e, `Stop
      ) (`Ok added_profiles) profiles in
  print @@ Printf.sprintf 
    "Applying profile \"%s\" to opam repository \"%s\". \n"
    profile opam_repo_target;
  ignore(ok_or_fail @@ checkout_profile ~ssl_no_verify profile profiles_url);
  let set = ok_or_fail @@ add_profiles added_profiles in
  ignore(ok_or_fail @@ add_pins profile);
  ignore(ok_or_fail @@ install_depexts ~ssl_no_verify profile);
  ignore(ok_or_fail @@ install_packages ~ssl_no_verify profile);`Ok set


let show_profile ?(depth=0) ~follow_profiles ~ssl_no_verify profile profiles_url = 
  ignore(ok_or_fail(checkout_profile ~ssl_no_verify profile profiles_url));
(*  let profiles' = load_profiles profile profiles_url in*)
  let open Printf in
  sprintf "Profile \"%s\":\n" profile ^
  " profiles:\n\t" ^
    (String.concat "\n\t" (profiles profile)) ^
  "\n pins:\n\t" ^
    (String.concat "\n\t" (pins profile |> List.map Pin_entry.to_string)) ^
  "\n packages:\n\t" ^
    (String.concat " " (packages profile)) ^
  "\n depexts:\n\t" ^
    (String.concat " " (depexts profile)) |>
  fun s -> print_endline s;`Ok (Operation.to_string (Operation.Show s))


let run operation profile opam_repo_target compiler_version profiles_url ssl_no_verify 
  list_profiles_flag follow_profiles =
  let op = Operation.of_string ~list_profiles_flag operation profile in
  let op_to_string = Operation.to_string op in
  match op with 
  | Operation.List ->
    begin 
      match list_profiles profiles_url with `Ok o -> `Ok op_to_string | `Error _ as e -> e 
    end
  | Operation.Apply profile ->
    begin
      ignore(ok_or_fail @@ opam_switch ~ssl_no_verify profile compiler_version);
      ignore(ok_or_fail(_run StringSet.empty profile opam_repo_target profiles_url ssl_no_verify));
      `Ok op_to_string
    end
  | Operation.Show profile ->
    show_profile ~follow_profiles ~ssl_no_verify profile profiles_url

let cmd =
  let doc = "Apply an ocaml profile to a target opam repository" in
  Term.(ret (pure run $ operation $ profile $ opam_repo_target $ compiler_version $ profiles_repo_url $ 
      ssl_no_verify $ list_profiles_flag $ follow_profiles)),
  Term.info "ocaml-profiles" ~version:"1.0" ~doc

let safe_cmd =
  try
    ignore(Term.eval cmd);
    clean_profiles_dir()
  with e -> ignore(clean_profiles_dir()); raise e

let () =
  ignore(clean_profiles_dir());
  match safe_cmd with `Error _ -> exit 1 | _ -> exit 0
