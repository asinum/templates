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


(* README.md generator *)

let yaml = Yaml_unix.of_file_exn (Fpath.v "meta.yml")

(* Intro *)

let fullname =
  match find "fullname" yaml with
  | Some yaml_value -> Yaml.Util.to_string_exn yaml_value
  | None -> "Unvalid fullname"


let shortname =
  match find "shortname" yaml with
  | Some yaml_value -> Yaml.Util.to_string_exn yaml_value
  | None -> "Unvalid shortname"


let organization =
  match find "organization" yaml with
  | Some yaml_value -> Yaml.Util.to_string_exn yaml_value
  | None -> "Unvalid organization"


let branch =
  match find "branch" yaml with
  | Some yaml_value -> Yaml.Util.to_string_exn yaml_value
  | None -> "master"


let travis = if get_bool "travis" yaml then
    "[![Travis][travis-shield]][travis-link]  " ^ "[travis-shield]: https://travis-ci.com/" ^ organization ^ "/" ^ shortname ^ ".svg?branch=" ^ branch ^ "  [travis-link]: https://travis-ci.com/" ^ organization ^ "/" ^ shortname ^"/builds   "
  else
    ""



let action = if get_bool "action" yaml then
    let docker_head = "[![Docker CI][docker-action-shield]][docker-action-link]" in
    let docker_body = "[docker-action-shield]: https://github.com/" ^  organization ^ "/" ^ shortname ^ "/workflows/Docker%20CI/badge.svg?branch=" ^ branch ^ "\n[docker-action-link]: https://github.com/" ^ organization ^ "/" ^ shortname ^ "/actions?query=workflow:\"Docker%20CI\"" in
    let nix = if  get_bool "nix" yaml then
        "[![Nix CI][nix-action-shield]][nix-action-link] \n [nix-action-shield]: https://github.com/" ^ organization ^ "/" ^ shortname ^ "/workflows/Nix%20CI/badge.svg?branch=" ^ branch ^"\n  [nix-action-link]: https://github.com/" ^ organization ^ "/" ^ shortname ^ "/actions?query=workflow:\"Nix%20CI\"" else "" in
    docker_head ^ "\n" ^ nix ^ "\n\n" ^ docker_body ^ "\n"
  else
    ""


let community = if get_bool "community" yaml then
    let head = "[![Contributing][contributing-shield]][contributing-link] \n [![Code of Conduct][conduct-shield]][conduct-link] \n [![Zulip][zulip-shield]][zulip-link] \n" in
    let contribute_body = "[contributing-shield]: https://img.shields.io/badge/contributions-welcome-%23f7931e.svg \n [contributing-link]: https://github.com/coq-community/manifesto/blob/master/CONTRIBUTING.md" in
    let conduct_body = "[conduct-shield]: https://img.shields.io/badge/%E2%9D%A4-code%20of%20conduct-%23f15a24.svg \n [conduct-link]: https://github.com/coq-community/manifesto/blob/master/CODE_OF_CONDUCT.md" in
    let zulip_body = "[zulip-shield]: https://img.shields.io/badge/chat-on%20zulip-%23c1272d.svg \n [zulip-link]: https://coq.zulipchat.com/#narrow/stream/237663-coq-community-devs.20.26.20users" in
    head ^ "\n\n" ^ contribute_body ^ "\n" ^ conduct_body ^ "\n" ^ zulip_body ^ "\n\n"
  else
    ""


let circle = if get_bool "circleci" yaml then
    "[![CircleCI][circleci-shield]][circleci-link] \n [circleci-shield]: https://circleci.com/gh/" ^ organization ^ "/" ^ shortname ^ ".svg?style=svg \n [circleci-link]:   https://circleci.com/gh/" ^ organization ^ "/" ^ shortname ^ "\n\n"
  else
    ""


let coqdoc = if get_bool "coqdoc" yaml then
    let coqdoc_index = match find "coqdoc_index" yaml with
      | Some yaml_value -> Yaml.to_string_exn yaml_value
      | None -> "/CoqDoc_Index_Missing"
    in
    let coqdoclink = if get_bool "community" yaml then
        "[coqdoc-link]: https://coq-community.org/" ^ shortname ^ coqdoc_index
      else
        "[coqdoc-link]: https://" ^ organization ^ ".github.io" ^ shortname ^ coqdoc_index
    in "[![coqdoc][coqdoc-shield]][coqdoc-link] \n [coqdoc-shield]: https://img.shields.io/badge/docs-coqdoc-blue.svg \n " ^ coqdoclink ^ "\n\n"
  else
    ""


let doi = match find "doi" yaml with
  | Some yaml_value -> "[![DOI][doi-shield]][doi-link] \n [doi-shield]: https://zenodo.org/badge/DOI/" ^ (Yaml.to_string_exn yaml_value) ^ ".svg \n [doi-link]: https://doi.org/" ^ (Yaml.to_string_exn yaml_value) ^ "\n\n"
  | None -> ""

let chat = match find "chat" yaml with
  | Some yaml_value ->
      let shield = match find "shield" yaml_value with
        | Some shield -> "[chat-shield]: https://img.shields.io/badge/" ^ (Yaml.to_string_exn shield) ^ "-join_chat-brightgreen.svg"
        | None -> ""
      in
      let link = match find "url" yaml_value with
        | Some link -> "[chat-link]: " ^ (Yaml.to_string_exn link)
        | None -> ""
      in
      "[![Chat][chat-shield]][chat-link] \n" ^ shield ^ "\n" ^ link ^ "\n\n"
  | None -> ""




let description =
  match find "description" yaml with
  | Some yaml_value -> Yaml.Util.to_string_exn  yaml_value
  | None -> "Missing description"


let intro = "# " ^ fullname ^ "\n\n" ^ travis ^ action ^ circle ^ community ^ coqdoc ^ doi ^ chat ^ description ^ "\n\n"



(* Meta *)
let author =
  let authors = find_list "authors" yaml in
  let f value =
    let initial = if get_bool "initial" value then  " (initial) \n" else "\n" in
    let name = match find "name" value with
      | Some yaml_value -> " - " ^ (Yaml.Util.to_string_exn yaml_value)
      | None -> "Missing author name"
    in
    name ^ initial
  in
  aux f authors "- Author(s) : \n"



let maintainer =
  let maintainers = find_list "maintainers" yaml in
  let f value =
    let name = match find "name" value with
      | Some yaml_value -> " - " ^ (Yaml.Util.to_string_exn yaml_value)
      | None -> "Missing maintainer name"
    in
    let nickname = match find "nickname" value with
      | Some yaml_value -> Yaml.Util.to_string_exn yaml_value
      | None -> "Missing nickname"
    in
    name ^ " ([**@" ^ nickname ^ "**](https://github.com/" ^ nickname ^ "))\n"
  in
  aux f maintainers "- Coq-community maintainer(s) : \n"


let license =
  let license_aux = match find "license" yaml with
    | Some yaml_value -> yaml_value
    | None -> `Null
  in
  let fullname = match find "fullname" license_aux with
    | Some value -> Yaml.Util.to_string_exn value
    | None -> "Missing license name"
  in
  let file = match find "file" license_aux with
    | Some value -> Yaml.Util.to_string_exn value
    | None -> "LICENSE"
  in " - License: [" ^ fullname ^ "](" ^ file ^ ") \n"


let version =
  let version_aux = match find "supported_coq_versions" yaml with
    | Some value -> value
    | None -> `Null
  in
  let text = match find "text" version_aux with
    | Some value -> Yaml.Util.to_string_exn value
    | None -> "MISSING SUPPORTED COQ VERSION"
  in
  "- Compatible Coq versions: " ^ text

let supported_coq_version = match find "supported_coq_versions" yaml with
  | None -> "a"
  | Some yaml_value -> match find "text" yaml_value with
    | Some value -> " - Compatible Coq versions: " ^ (Yaml.Util.to_string_exn value) ^ "\n\n"
    | None -> " - Compatible Coq versions:  Not available information \n\n"

let supported_ocaml_version = match find "supported_ocaml_versions" yaml with
  | None -> "b"
  | Some yaml_value -> match find "text" yaml_value with
    | Some value -> " - Compatible OCaml versions: " ^ (Yaml.Util.to_string_exn value) ^ "\n\n"
    | None -> " - Compatible OCaml versions:  Not available information \n\n"

let dependency =
  let dependencies = find_list "dependencies" yaml in
  match dependencies with
  | [] -> " - Additional dependencies: 'None' "
  | _ ->
      let f value = match find "description" value with
        | Some yaml_value -> " -" ^ Yaml.Util.to_string_exn yaml_value
        | None ->  "No description available"
      in aux f dependencies " - Additional dependencies:"


let namespace =
  let packages = find_list "packages" yaml in
  let rec temp list acc = match list with
    | [] -> acc ^ ".\n\n"
    | h::m::q -> let res = match find "namespace" h with
        | Some value -> temp (m::q) (acc ^ "'" ^ (Yaml.Util.to_string_exn value) ^ "', ")
        | _ -> temp (m::q) (acc ^ "package without a namespace" ^ ", ")
        in res
    | h::q -> match find "namespace" h with
      | Some value -> temp q (acc ^ "'" ^ (Yaml.Util.to_string_exn value) ^ "'")
      | None -> temp q (acc ^ "package without a namespace" ^ ", ")
  in
  temp packages " - Coq namespace : "

let publication =
  let publications = find_list "publications" yaml in
  let f value =
    let title = match find "pub_title" value with
      | Some yaml_value -> " - [" ^ (Yaml.Util.to_string_exn yaml_value) ^ "]"
      | None -> "Missing publication title"
    in
    let url = match find "pub_url" value with
      | Some yaml_value -> "(" ^ (Yaml.Util.to_string_exn yaml_value) ^ ")"
      | None -> "Missing publication url"
    in
    let doi = match find "pub_doi" value with
      | Some yaml_value -> " doi:[" ^ (Yaml.Util.to_string_exn yaml_value) ^ "](https://doi.org/" ^ (Yaml.Util.to_string_exn yaml_value) ^ ") \n"
      | None -> "\n"
    in
    title ^ url ^ doi
  in aux f publications "- Related publication(s):"


let meta = " ## Meta \n\n" ^ author ^ "\n" ^ maintainer ^ license ^ supported_coq_version ^ supported_ocaml_version ^ dependency ^ "\n" ^ namespace ^ publication



(* Build *)
let build =
  match find "build" yaml with
  | Some yaml_value -> Yaml.Util.to_string_exn  yaml_value
  | None ->
      let opam_name = match find "opan_name" yaml with
        | Some value -> Yaml.Util.to_string_exn value
        | None -> "coq-"^shortname
      in
      let automatic = "## Building and installation instructions \n\n" ^ "The easiest way to install the latest released version of " ^ fullname ^ " is via [OPAM](https://opam.ocaml.org/doc/Install.html):\n" ^ "```shell \nopam repo add coq-released https://coq.inria.fr/ \nopam install " ^ opam_name ^ " \n```" in
      let make_target = "" in
      let manual = "\n\nTo instead build and install manually, do:\n``` shell \n git clone "^ "https://github.com/" ^ organization ^ "/" ^ shortname ^ ".git \ncd " ^ shortname in
      let dune = if get_bool "dune" yaml
        then "dune build \ndune install"
        else
          "make {" ^ make_target ^ " # or make -j <number-of-cores-on-your-machine> " ^ make_target ^ "\nmake install \n```"
      in
      automatic ^ manual ^ "\n" ^ dune ^ "\n\n"


(* Documentation *)

let documentation =
  match find "documentation" yaml with
  | Some yaml_value -> Yaml.Util.to_string_exn  yaml_value
  | None -> ""




(* Execution *)


let file = "README.md"
let result = intro ^ meta ^ build ^ documentation

let () =
  let oc = open_out(file) in
  Printf.fprintf oc "%s\\n" result;
  close_out oc;
