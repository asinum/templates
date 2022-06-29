(* General functions *)


let find key (yaml : Yaml.value) = match yaml with
  | `O assoc -> List.assoc_opt key assoc
  | _ -> None


let find_list key (yaml : Yaml.value) = match find key yaml with
  | Some `A list -> list
  | _ -> []


let rec aux f list acc = match list with
  | [] -> acc
  | h::q ->
      let temp = f h
      in aux f q (acc ^ " \n" ^ temp  )


(* Definition of global var *)

let yaml = Yaml_unix.of_file_exn (Fpath.v "meta.yml")
let file = "dune"
let intro = "; This file was generated from `meta.yml`, please do not edit manually. \n" ^"; Follow the instructions on https://github.com/coq-community/templates to regenerate.\n"


(* Code *)

let generator package =
  let name = match find "namespace" package with
    | Some value -> "  (name " ^ (Yaml.Util.to_string_exn value) ^ ")\n"
    | None -> "  (name Missing namespace)\n"
  in
  let packaging = match find "shortname" package with
    | Some value -> "  (package coq-" ^ (Yaml.Util.to_string_exn value) ^ ")\n"
    | None -> "  (package Missing package shortname)\n"
  in
  let synopsis = match find "synopsis" package with
    | Some value -> "  (synopsis \"" ^ (Yaml.Util.to_string_exn value) ^ "\"))\n"
    | None -> "  (synopsis Missing synopsis))\n"
  in "(coq.theory \n" ^ name ^ packaging ^ synopsis



let result = intro ^ (aux generator (find_list "packages" yaml) "")


let () =
  let oc = open_out(file) in
  Printf.fprintf oc "%s" result;
  close_out oc;
