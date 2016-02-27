module Opt = CCOpt
module Var = struct
  type t = {name:string;value:string} [@@deriving show]
end

let ssl_no_verify_env_name = "GIT_SSL_NO_VERIFY"
let ssl_no_verify_var = function
  | None -> None
  | Some b -> Some Var.{name=ssl_no_verify_env_name; value=string_of_bool b}

let opam_yes_env_name = "OPAMYES"
let opam_yes_var t =
  let value = Opt.map (fun b -> string_of_bool b) t 
  |> Opt.get_lazy (fun () -> string_of_bool true) in
  Some Var.{name=opam_yes_env_name; value}

let putenv var = Unix.putenv var.Var.name var.Var.value
let getenv var = try Some (Unix.getenv var.Var.name) with _ -> None

let with_envs envs (f:unit->'a) =
  let prev_envs = CCList.filter_map
    (fun var -> getenv var |>
      CCOpt.map (fun value -> Var.{var with value})) envs in
  List.iter putenv envs;
  try
    let ret = f () in
    `Ok ret
  with e ->
  begin
      List.iter putenv prev_envs;
      `Error (* (false, Printexc.to_string e) *)
      e
  end

let with_env ?env f =
  match env with
  | None -> `Ok (f ())
  | Some env -> with_envs [env] f

let with_env_opts ?ssl_no_verify ?opam_yes f =
  let envs =
    (CCOpt.to_list @@ ssl_no_verify_var ssl_no_verify)
    @
    CCOpt.to_list @@ opam_yes_var opam_yes in
  match with_envs envs f with
    | `Ok (`Ok o) -> `Ok o
    | `Ok (`Error e) -> `Error e
    | `Error _ as e -> Errors.to_tuple e
