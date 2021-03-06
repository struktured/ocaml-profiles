module PIN = OpamClient.SafeAPI.PIN
module Shell = Shell_support.Shell
open Ocaml_profiles_constants
module List = CCList
let print = Debug.print

module Kind = struct
  type t = OpamTypes.pin_kind
  let to_string = function `git -> "git" | `hg -> "hg"
   | `darcs -> "darcs" | `version -> "version"
   | `local -> "local" | `http -> "http"
  let of_string s = match String.lowercase s with
   | "git" -> `git
   | "hg" -> `hg
   | "darcs" -> `darcs
   | "local" -> `local
   | "version" -> `version
   | "http" -> `http
   | k -> failwith("unknown kind: " ^ k)
end


module Pin_entry = struct
  type t = {name:string;kind:Kind.t; target:string}
  let to_string t = Printf.sprintf "%s %s %s" t.name
    (Kind.to_string t.kind) t.target
  let should_reinstall t = match t.kind with
    | `git | `hg | `darcs |`local | `http -> true
    | `version -> false
end

let pinned_config_file profile =
    FilePath.concat
        (Profiles.profile_dir profile)
        pinned_file_name

let pinned_config_file_target opam_repo_target compiler_version
  = FilePath.concat opam_repo_target @@
    FilePath.concat compiler_version pinned_file_name

let for_profile profile =
  let open Pin_entry in
  pinned_config_file profile |>
  Errors.if_sys_error ~then_default:[] ~f:Shell.lines_of_file |>
  List.map String.trim |>
  List.filter (fun s -> String.length s > 0) |>
  List.map (fun s -> Re.split (Re_posix.compile_pat " ") s) |>
  List.map (function [name;kind;target] ->
             {name;kind=Kind.of_string kind;target} |
               l -> failwith("unxpected number of columns for line: " ^
                                 String.concat " " l))

let pins = Errors.if_sys_error ~then_default:[]
  ~f:(fun profile ->
    print @@ Printf.sprintf "getting pins for profile %s " profile;
    for_profile profile)

let add_pin = let open Pin_entry in
  function {name;kind;target} as entry ->
      let package_name = OpamPackage.Name.of_string name in
      let version = None in
      let pin_option = match kind with
       | `darcs -> OpamTypes.Darcs (target, None)
       | `git -> OpamTypes.Git (target, None)
       | `local -> OpamTypes.Local (OpamFilename.dirname @@ OpamFilename.of_string target)
       | `hg -> OpamTypes.Hg (target, None)
       | `http -> OpamTypes.Http (target, None)
       | `version -> OpamTypes.Version (OpamPackage.Version.of_string target) in
      try
        PIN.pin package_name ~edit:false ~action:true ?version
          (Some pin_option);
        `Ok entry (*(Printf.sprintf "%s pinned" name) *)
      with e -> `Error (false, Printexc.to_string e)

let remove_pins pins =
  let open Pin_entry in
  let package_names_as_str = List.map
    (fun {name;kind;target} -> name) pins in
  let package_names = List.map
    (fun name -> OpamPackage.Name.of_string name) package_names_as_str in
  try
    PIN.unpin ~action:true package_names;
    `Ok (Printf.sprintf "[%s] pins removed"
      (String.concat ", " package_names_as_str))
  with e -> `Error (false, Printexc.to_string e)

let apply profile =
  print @@ Printf.sprintf "add_pins: %s" profile;
  let open Pin_entry in
  let pins = pins profile in
  let open Shell_support.Shell.Infix in
  remove_pins pins >>= fun ok -> print ok;
  List.fold_while (fun res pin ->
      match res with
       | `Error _ as e -> e, `Stop
       | `Ok l ->
           begin
             match (add_pin pin) with 
          | `Ok e -> `Ok (e::l), `Continue
          | `Error _ as e ->  e, `Stop
          end) (`Ok []) pins
