let find key (yaml : Yaml.value) = match yaml with
  | `O assoc -> List.assoc_opt key assoc
  | _ -> None


let yaml = Yaml_unix.of_file_exn (Fpath.v "meta.yml")
let shortname = match find "shortname" yaml with
  | Some value -> Yaml.Util.to_string_exn value
  | None -> "Missing shortname"
let result = "(lang dune 2.5) \n" ^ "(using coq 0.2)\n" ^ "(name " ^ shortname ^ ")"

let file = "dune-project"

let () =
  let oc = open_out(file) in
  Printf.fprintf oc "%s" result;
  close_out oc;
