open Core.Std;;

type analyzer = {
  sid: string;
  cwd: string;
  project_dir: string;
  global_table: Type.state_t;
  all_bindings: Type.binding_ty list;
  loaded_files: (string, bool) Hashtbl.t;
  references: (Node.node, Type.binding_ty list) Hashtbl.t;
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
    references = Hashtbl.Poly.create ();
    path = [];
  }

let global_Analyzer = make_analyzer();;

let put_refs node bs =
  let binded = Hashtbl.find global_Analyzer.references node in
  match binded with
  | None ->  (
      Hashtbl.add_exn global_Analyzer.references ~key:node ~data: bs
    )
  | Some(v) -> (
      List.iter bs ~f:(fun b -> Type.binding_add_ref b node);
      ignore(Hashtbl.replace global_Analyzer.references ~key:node ~data:(v @ bs));
    )

let put_ref node bind =
  let bs = [bind] in
  put_refs node bs

