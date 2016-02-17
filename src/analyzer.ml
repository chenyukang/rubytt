open Trans
open Util
open Parser
open Global

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
