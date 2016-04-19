module O = OpamClient.SafeAPI
module Shell = Shell_support.Shell
open Ocaml_profiles_constants
let print = Debug.print

let package_config_file profile = FilePath.concat (Profiles.profile_dir profile)
    package_file_name

let for_profile profile =
  try 
    package_config_file profile |>
    Shell.lines_of_file |> List.map (fun s -> Re.split (Re_posix.compile_pat " ") s)
                                              |> List.flatten |>
   CCList.filter_map (fun s -> match String.trim s with "" -> None | s -> Some s)
  with _ -> []

let remove ?(force=true) packages =
begin
  let atoms : OpamTypes.atom list = List.map 
  (fun p -> (OpamPackage.Name.of_string p, None)) packages in
  O.remove ~autoremove:false ~force atoms;
  `Ok (Printf.sprintf "remove: %s" (String.concat ", " packages))
end

let remove_for_profile ?ssl_no_verify ?(force=true) profile =
  let packages = for_profile profile in
  match packages with
  | [] -> `Ok (Printf.sprintf "No packages to install for %s" profile) | _ ->
  let remove () = remove ~force packages in
  Env_options.with_env_opts ?ssl_no_verify remove

let install ?ssl_no_verify profile =
  let packages = for_profile profile in
  match packages with
  | [] -> `Ok ("No packages to install for " ^ profile) | _ ->
 let install () =
   begin
    let atoms : OpamTypes.atom list = List.map
      (fun p -> (OpamPackage.Name.of_string p, None)) packages in
    O.install atoms None false;
    `Ok (Printf.sprintf "install: %s" (String.concat ", " packages))
    end in
  Env_options.with_env_opts ?ssl_no_verify install


