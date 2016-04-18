module O = OpamClient.SafeAPI
open Arguments
open Cmdliner
open Errors
module List = CCList
let default_opam_root = try
  FilePath.concat (Unix.getenv "HOME") ".opam"
  with _ -> "~/.opam"

let default_profiles_url = "https://github.com/struktured/ocaml-profiles.git"

let default_opam_yes = try
  bool_of_string (Unix.getenv "OPAMYES")
  with _ -> true

type t = {opam_root:string; profiles_url : string} [@@deriving show]

let with_opam_root
  ?(opam_root=default_opam_root)
  ?(profiles_url=default_profiles_url)
  ?(opam_yes=default_opam_yes )
  f =
  let previous_root = !OpamGlobals.root_dir in
  let previous_yes = !OpamGlobals.yes in
  begin
    OpamGlobals.yes := opam_yes;
    OpamGlobals.root_dir := opam_root;
    let ret = f {opam_root;profiles_url} in
    OpamGlobals.yes := previous_yes;
    OpamGlobals.root_dir := previous_root;
    ret
  end

let rec _run added_profiles profile opam_repo_target profiles_url ssl_no_verify =
  let add_profiles added_profiles = let open Profiles in
  let profiles = Profiles.load_profiles profile profiles_url in
  List.fold_while
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
  let pin_entries = ok_or_fail @@ Pins.apply profile in
  let should_reinstall = let open Pins.Pin_entry in function
    | {name;_} as entry when should_reinstall entry -> Some name
    | _ -> None in
  let to_reinstall = CCList.filter_map should_reinstall pin_entries in
  ignore(ok_or_fail @@ Packages.remove to_reinstall);
  ignore(ok_or_fail @@ Packages.install ~ssl_no_verify profile);`Ok set

let show_profile ?(depth=0) ~follow_profiles ~ssl_no_verify profile profiles_url = 
  ignore(ok_or_fail(Profiles.checkout_profile ~ssl_no_verify profile profiles_url));
(*  let profiles' = load_profiles profile profiles_url in*)
  let open Printf in
  sprintf "Profile \"%s\":\n" profile ^
  " profiles:\n\t" ^
    (String.concat "\n\t" (Profiles.for_profile profile)) ^
  "\n pins:\n\t" ^
    (String.concat "\n\t" (Pins.for_profile profile |> List.map Pins.Pin_entry.to_string)) ^
  "\n packages:\n\t" ^
    (String.concat " " (Packages.for_profile profile)) ^
  "\n depexts:\n\t" ^
    (String.concat " " (Depexts.for_profile profile)) |>
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
  | Operation.Apply profile -> Env_options.with_env_opts ~ssl_no_verify (fun () -> with_opam_root (fun _ ->
    begin
      ignore(ok_or_fail @@ opam_switch ~ssl_no_verify profile compiler_version);
      ignore(ok_or_fail(_run StringSet.empty profile opam_repo_target profiles_url ssl_no_verify));
      `Ok op_to_string
    end))
  | Operation.Show profile ->
    show_profile ~follow_profiles ~ssl_no_verify profile profiles_url

let cmd () =
  let doc = "Apply an ocaml profile to a target opam repository" in
  Term.(ret (pure run $ operation $ profile $ opam_repo_target $ compiler_version $ profiles_repo_url $ 
      ssl_no_verify $ list_profiles_flag $ follow_profiles)),
  Term.info "ocaml-profiles" ~version:"1.0" ~doc

let safe_cmd () =
  try
    ignore(Term.eval @@ cmd ());
    Profiles.clean_profiles_dir ()
  with e -> ignore(Profiles.clean_profiles_dir ()); raise e

let run ?opam_root ?profiles_url () =
  ignore(Profiles.clean_profiles_dir ());
  let f = safe_cmd () |> function
    | `Error _ -> exit 1
    | _ -> exit 0 in
  with_opam_root ?opam_root ?profiles_url f
