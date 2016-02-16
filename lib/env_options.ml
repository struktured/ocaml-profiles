module Var = struct
  type t = {name:string;value:string} [@@deriving show]
end
let ssl_no_verify_env_name = "GIT_SSL_NO_VERIFY"
let ssl_no_verify_var = function
  | None -> None
  | Some b -> Some Var.{name=ssl_no_verify_env_name; value=string_of_bool b}

let putenv var = Unix.putenv var.Var.name var.Var.value
let getenv var = Unix.getenv var.Var.name

let with_envs envs (f:unit->'a) = 
  let prev_envs = List.map 
    (fun var -> Var.{var with value=getenv var}) envs in
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

let with_env_opts ?ssl_no_verify f =
  match with_env ?env:(ssl_no_verify_var ssl_no_verify) f
  with `Ok (`Ok o) -> `Ok o | `Ok (`Error e) -> `Error e | `Error _ as e -> Errors.to_tuple e
 

