(* General functions *)

let find key (yaml : Yaml.value) = match yaml with
  | `O assoc -> List.assoc_opt key assoc
  | _ -> None


let get_bool key (yaml : Yaml.value) = match find key yaml with
  | Some bool -> Yaml.Util.to_bool_exn bool
  | _ -> false


let find_list key (yaml : Yaml.value) = match find key yaml with
  | Some `A list -> list
  | _ -> []


let rec aux f list acc = match list with
  | [] -> acc
  | h::q ->
      let temp = f h
      in aux f q (acc ^ "  \n " ^ temp  )


(* coq-*.opam generator *)

let yaml = Yaml_unix.of_file_exn (Fpath.v "meta.yml")

let organization = match find "organization" yaml with
  | Some value -> Yaml.Util.to_string_exn value
  | None -> "No valid organization"











let generator package =
  let intro = "# This file was generated from `meta.yml`, please do not edit manually. \n" ^ "# Follow the instructions on https://github.com/coq-community/templates to regenerate.\n\n" in
  let opam_version = "opam_version: \"2.0\" \n" in
  let maintainer = match find "opam-file-maintainer" package with
    | Some value -> "maintainer: \"" ^ (Yaml.Util.to_string_exn value) ^ "\"\n"
    | None -> "maintainer: \"palmskog@gmail.com\" \n"
  in
  let version = match find "opam-file-version" package with
    | Some value -> "version: \"" ^ (Yaml.Util.to_string_exn value) ^ "\"\n"
    | None -> "version: \"dev\" \n"
  in
  let shortname = match find "shortname" yaml with
    | Some value -> Yaml.Util.to_string_exn value
    | None -> "No valid shortname"
  in

  let description = match find "description" package with
    | Some value -> "description: \"\"\"" ^ (Yaml.Util.to_string_exn value) ^ "\"\"\"\n"
    | None -> "description:\"\"\" \n"
  in
  let homepage = "homepage: \"https://github.com/" ^ organization ^ "/" ^ shortname ^ "\"\n"in
  let dev_repo = "dev-repo: \"git+https://github.com/" ^ organization ^ "/" ^ shortname ^ ".git\"\n" in
  let bug_reports = "bug-reports: \"https://github.com/" ^ organization ^ "/" ^ shortname ^ "/issues\" \n" in
  let doc = if get_bool "coqdoc" yaml then "doc: \"https://" ^ organization ^ ".github.io/" ^ shortname ^"/\" \n" else "" in
  let license = match find "license_identifier" package with
    | Some value -> "license: \"" ^ (Yaml.Util.to_string_exn value) ^ "\"\n"
    | None -> "No valid license \n"
  in
  let synopsis =  match find "synopsis" package with
    | Some value -> "synopsis: \"" ^ (Yaml.Util.to_string_exn value) ^ "\"\n"
    | None -> "No valid synopsis"
  in
  let dune =
    if get_bool "dune" package then
      let build  = " build: [\"dune\" \"build\" \"-p\" name \"-j\" jobs] \n" in
      let make_target = match find "make_target" package with
        | Some value -> " build: [make \"-j%{jobs}%\"" ^ (Yaml.Util.to_string_exn value) ^ "\"{{/ make_target }}] \n"
        | None -> ""
      in
      let test_target = match find "test_target" package with
        | Some value -> " run-test: [make \"-j%{jobs}%\" \"" ^(Yaml.Util.to_string_exn value) ^ "\"] \n"
        | None -> ""
      in
      let install =  match find "install_flag" package with
        | Some value -> " install: [make \"install\" \"" ^(Yaml.Util.to_string_exn value) ^ "\"] \n"
        | None -> ""
      in
      build ^ make_target ^ test_target ^ install
    else ""
  in
  let depend =
    let ocaml =
      let version = match find "supported_ocaml_versions" package with
        | Some value -> value
        | None -> `Null
      in
      match find "opam" version with
      | Some value ->  " \"ocaml\"" ^ (Yaml.Util.to_string_exn value) ^ "\n"
      | None -> ""
    in
    let dune = if get_bool "dune" package then " \"dune\" {>= \"2.5\"} \n" else ""
    in
    let coq =
      let version = match find "supported_coq_versions" package with
        | Some value -> value
        | None -> `Null
      in
      match find "opam" version with
      | Some value ->  " \"coq\"" ^ (Yaml.Util.to_string_exn value)
      | None -> ""
    in
    let dependency =
      let dependencies = find_list "dependencies" package in
      let f value =
        let value_aux = match find "opam" value with
          | Some yaml_value -> yaml_value
          | None -> `Null
        in
        let name = match find "name" value_aux with
          | Some yaml_value -> "\"" ^ (Yaml.Util.to_string_exn yaml_value) ^ "\""
          | None -> "Missing dependency name"
        in
        let version = match find "version" value_aux with
          | Some yaml_value -> (Yaml.Util.to_string_exn yaml_value)
          | None -> ""
        in
        name ^ version
      in
      aux f dependencies ""
    in
    "depends: [ \n" ^ ocaml ^ dune ^ coq ^ dependency ^ "\n]\n"
  in
  let author =
    let authors = find_list "authors" package in
    let f value =
      let email = match find "email" value with
        | Some yaml_value -> "<" ^ (Yaml.Util.to_string_exn yaml_value) ^ ">"
        | None -> ""
      in
      let name = match find "name" value with
        | Some yaml_value -> "\"" ^ (Yaml.Util.to_string_exn yaml_value) ^ "\""
        | None -> "Missing author name"
      in
      name ^ email
    in
    "authors: [" ^ (aux f authors "") ^ "\n]"
  in
  let tags =
    let category =
      let categories = find_list "categories" package in
      let f value = match find "name" value with
        | Some yaml_value -> "\"category: " ^ (Yaml.Util.to_string_exn yaml_value) ^ "\""
        | None -> "Missing category name"
      in
      aux f categories ""
    in
    let keyword =
      let keywords = find_list "keywords" package in
      let f value = match find "name" value with
        | Some yaml_value -> "\"keyword: " ^ (Yaml.Util.to_string_exn yaml_value) ^ "\""
        | None -> "Missing keyword name"
      in
      aux f keywords ""
    in
    let date = match find "date" package with
      | Some value -> "\"date: " ^ (Yaml.Util.to_string_exn value) ^ "\""
      | None -> ""
    in
    let namespace = match find "namespace" package with
      | Some value -> " \"logpath: " ^ (Yaml.Util.to_string_exn value) ^ "\""
      | None -> ""
    in
    "tags: [" ^ category ^ keyword ^ "\n" ^ date ^ namespace ^ "\n]\n"
  in
  intro ^ opam_version ^ maintainer ^ version ^ "\n" ^ homepage ^ dev_repo ^ bug_reports ^ doc ^ license ^ "\n" ^ synopsis ^ description ^ "\n" ^ dune ^ depend ^ "\n" ^ tags ^ "\n" ^ author


    (* Main *)

let packages = find_list "packages" yaml
let rec aux list = match list with
  | [] -> ()
  | h::q ->
      let file = match find "shortname" h with
        | Some value -> "coq-" ^ (Yaml.Util.to_string_exn value) ^ ".opam"
        | None -> "Missing_package_name.opam"
      in
      let result = generator h in
      let oc = open_out(file) in
      Printf.fprintf oc "%s" result;
      close_out oc;
      aux q
let () = aux packages
