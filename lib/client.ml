module O = OpamClient.SafeAPI
open Arguments
open Cmdliner
let default_opam_root = FilePath.concat (Unix.getenv "HOME") ".opam"
let default_profiles_url = "https://github.com/struktured/ocaml-profiles.git"

type t = {opam_root:string; profiles_url : string} [@@deriving show]

let with_opam_root ?(opam_root=default_opam_root) ?(profiles_url=default_profiles_url) () f : t =
  let previous_root = !OpamGlobals.root_dir in
  OpamGlobals.root_dir := opam_root; 
  let ret = f {opam_root;profiles_url} in
  OpamGlobals.root_dir := previous_root;
  ret

let rec _run added_profiles profile opam_repo_target profiles_url ssl_no_verify =
 let add_profiles added_profiles =
  let open Profiles in
  let profiles = Profiles.load_profiles profile profiles_url in
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
  Debug.print @@ Printf.sprintf 
    "Applying profile \"%s\" to opam repository \"%s\". \n"
    profile opam_repo_target;
  ignore(ok_or_fail @@ Profiles.checkout_profile ~ssl_no_verify profile profiles_url);
  let set = ok_or_fail @@ add_profiles added_profiles in
  ignore(ok_or_fail @@ Pins.apply profile);
  ignore(ok_or_fail @@ install_depexts ~ssl_no_verify profile);
  ignore(ok_or_fail @@ install_packages ~ssl_no_verify profile);`Ok set


let show_profile ?(depth=0) ~follow_profiles ~ssl_no_verify profile profiles_url = 
  ignore(ok_or_fail(Profiles.checkout_profile ~ssl_no_verify profile profiles_url));
(*  let profiles' = load_profiles profile profiles_url in*)
  let open Printf in
  sprintf "Profile \"%s\":\n" profile ^
  " profiles:\n\t" ^
    (String.concat "\n\t" (Profiles.profiles profile)) ^
  "\n pins:\n\t" ^
    (String.concat "\n\t" (Pins.pins profile |> List.map Pins.Pin_entry.to_string)) ^
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
      match Profiles.list_profiles profiles_url with `Ok o -> `Ok op_to_string | `Error _ as e -> e 
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
    Profiles.clean_profiles_dir()
  with e -> ignore(Profiles.clean_profiles_dir()); raise e

let () =
  ignore(Profiles.clean_profiles_dir());
  match safe_cmd with `Error _ -> exit 1 | _ -> exit 0
