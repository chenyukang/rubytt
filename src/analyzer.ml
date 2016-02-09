
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
    global_table = State.new_state ~parent:None State.Global;
    all_bindings = [];
    loaded_files = Hashtbl.create 1;
    path = [];
  }

let global_Analyzer = make_analyzer();;
let global_table() = global_Analyzer.global_table;;

