open Core.Std;;
open Type

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


let trans ast =
  Trans.transform_expr ast Type.global_table
  
                       
  

