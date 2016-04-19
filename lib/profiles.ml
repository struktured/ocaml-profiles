type profile = {name:string;url:string}
open Ocaml_profiles_constants
module Git = Shell_support.Git
module Shell = Shell_support.Shell
let print = Debug.print

let profiles_default_url () =
  try
    Unix.getenv "OCAML_PROFILES_URL"
 with Not_found -> "https://github.com/struktured/ocaml-profiles"

let profiles_branch_prefix = "profiles"

let profile_dir profile = 
  if FileUtil.test FileUtil.Is_dir profile then
    profile
  else
    FilePath.concat profiles_dir profile

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

let profiles_config_file profile = FilePath.concat (profile_dir profile)
    profiles_file_name

let for_profile profile =
  try
    profiles_config_file profile |>
    Shell.lines_of_file
  with _ -> []

let load_profiles profile url  =
  let profiles = for_profile profile in
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

let clean_profiles_dir () =
  `Ok (FileUtil.rm ~recurse:true ~force:FileUtil.Force [profiles_dir])


