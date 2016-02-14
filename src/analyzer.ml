open Trans
open Util
open Parser

type analyzer = {
  sid: string;
  cwd: string;
  project_dir: string;
  global_table: Type.state_t;
  all_bindings: Type.binding_ty list;
  loaded_files: (string, bool) Hashtbl.t;
  path: string list;
}

let make_analyzer () =
  {
    sid =  "";
    cwd =  "";
    project_dir = "";
    global_table = Type.global_table;
    all_bindings = [];
    loaded_files = Hashtbl.create 1;
    path = [];
  }

let global_Analyzer = make_analyzer();;

let is_global_name name =
  let open Core.Std in
  String.substr_index name "$" = Some(0)

let get_modulebinding_if_global st name =
  let res = ref None in
  if is_global_name name then
    if Type.global_table <> st then
        res := State.lookup_local Type.global_table name;
  !res


let load_file file =
  let json = run_dump_ruby file in
  let ast = build_ast_from_file json in
  ignore(Trans.transform_expr ast Type.global_table);
  let ast_str = Printer.node_to_str ast 0 in
  Printf.printf "%s\n" ast_str
