opam install -y depext
opam pin add -y -k git shell-support https://github.com/struktured/ocaml-shell-support#stable
opam pin add -y -k git ocaml-profiles https://github.com/struktured/ocaml-profiles#stable
opam reinstall -y shell-support ocaml-profiles
