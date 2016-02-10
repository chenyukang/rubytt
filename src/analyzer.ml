type analyzer = {
  sid: string;
  cwd: string;
  project_dir: string;
  global_table: Type.state_t;
  all_bindings: Type.bind_ty list;
  loaded_files: (string, bool) Hashtbl.t;
  path: string list;
}

let make_analyzer () =
  {
    sid =  "";
    cwd =  "";
    project_dir = "";
    global_table = State.global_table;
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

