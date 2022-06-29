#!/usr/bin/env bash



ocamlfind ocamlopt -o opam_run -linkpkg -package yaml.unix,yaml ./templates/opam.ml
ocamlfind ocamlopt -o readme_run -linkpkg -package yaml.unix,yaml ./templates/readMe.ml
ocamlfind ocamlopt -o dune_run -linkpkg -package yaml.unix,yaml ./templates/dune.ml
ocamlfind ocamlopt -o dune_project_run -linkpkg -package yaml.unix,yaml ./templates/dune_project.ml
if [ $# = 0 ]
then

  ./opam_run
  ./readme_run
  ./dune_project_run
  ./dune_run

  rm opam_run readme_run  dune_project_run dune_run
else
  for arg in "$@"
  do
    if ["$arg" = "dune"]
    then
      ./dune_run
      rm dune_run
    else
      continue
    fi
    if ["$arg" = "opam"]
    then
      ./opam_run
      rm opam_run
    else
      continue
    fi
    if ["$arg" = "dune_project"]
    then
      ./dune_project_run
      rm dune_project_run
    else
      continue
    fi
    if ["$arg" = "readme"]
    then
      ./readme_run
      rm readme_run
    else
      continue
    fi
  done
fi
rm ./templates/*.cmx ./templates/*.o ./templates/*.cmi
