module O = OpamClient.SafeAPI

let default_opam_root = FilePath.concat (Unix.getenv "HOME") ".opam"
let default_profiles_url = "https://github.com/struktured/ocaml-profiles.git"

type t = {opam_root:string; profiles_url : string} [@@deriving show]

let with_opam_root ?(opam_root=default_opam_root) ?(profiles_url=default_profiles_url) () f : t =
  let previous_root = !OpamGlobals.root_dir in
  OpamGlobals.root_dir := opam_root; 
  let ret = f {opam_root;profiles_url} in
  OpamGlobals.root_dir := previous_root;
  ret


