
module Strings = 
struct
let eq = "="
let opam = "opam"
let space = " "
let dash = "-" 
let empty = ""
let force = dash ^ "f"
let non_interactive = dash ^ "y"
let git_ssl_no_verify = "GIT_SSL_NO_VERIFY"
let _true = "true"
end

module Flag = 
struct
  open Strings
  type t =[ `Force | `Non_interactive | `Ssl_no_verify] [@@deriving show]

  let to_command_str = function
    | `Force -> force  
    | `Interactive -> non_interactive
    | `Ssl_no_verify -> git_ssl_no_verify ^ eq ^ _true
end

module Command = 
struct
  type t = [ `Install | `Remove | `Reinstall | `Pin ] [@@deriving show]
end

let command_string arguments

  opam ^ space ^ force
let switch ?force ?interactive 


