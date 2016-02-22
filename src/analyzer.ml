open Core.Std;;
open Type
open State

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
    loaded_files = Hashtbl.Poly.create ();
    path = [];
  }

let global_Analyzer = make_analyzer();;

let trans ast =
  ignore(Trans.transform_expr ast Type.global_table);
  Printf.printf "\n%s\n\n" (Printer.table_to_str Type.global_table 0)
