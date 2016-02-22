open Core.Std;;

type state_type =
  | Class
  | Instance
  | Function
  | Module
  | Global
  | Scope
and
  ('ty, 'binding) state = {
  mutable parent: ('ty, 'binding) state option;
  mutable supers: ('ty, 'binding) state option;
  mutable s_type: state_type;
  mutable t_type: 'ty option;
  mutable s_table: (string, 'binding list) Hashtbl.t;
  mutable path: string;
}

let new_state ?(parent = None) state_ty : ('ty, 'binding) state =
  {
    parent = parent;
    supers = None;
    s_type = state_ty;
    t_type = None;
    s_table = Hashtbl.create ~hashable:String.hashable ();
    path = "";
  }


let set_parent st parent = st.parent <- Some(parent)
let set_stype st stype = st.s_type <- stype
let set_ttype st ttype = st.t_type <- ttype
let set_path st path = st.path <- path
let set_supers st supers = st.supers <- supers
let parent st = st.parent
let s_type st = st.s_type

let remove st id =
  Hashtbl.remove st.s_table id

let state_keyset st =
  Hashtbl.keys st.s_table

let state_clear st =
  Hashtbl.clear st.s_table;
  st.supers <- None;
  st.parent <- None;
  st.t_type <- None;
  st.path <- ""

let state_copy st =
  {
    parent = st.parent;
    supers = st.supers;
    s_type = st.s_type;
    t_type = st.t_type;
    s_table = Hashtbl.copy st.s_table;
    path = st.path;
  }

let state_overwrite st st_v =
  st.s_table <- st_v.s_table;
  st.path <- st_v.path;
  st.parent <- st_v.parent;
  st.supers <- st_v.supers;
  st.t_type <- st_v.t_type;
  st.s_type <- st_v.s_type

(* let state_merge_with st1 st2 = *)
(*   st1 *)

(* let state_merge st1 st2 = *)
(*   let ret = state_copy st1 in *)
(*   ignore(state_merge_with ret st2); *)
(*   ret *)

let state_update st id bindings =
  match Hashtbl.add st.s_table ~key:id ~data:bindings with
  | `Duplicate -> Hashtbl.replace st.s_table ~key:id ~data:bindings
  | _ -> ()

let state_update_bind st id binding =
  state_update st id [binding]


let extend_path st name sep =
  let name = Util.main_name name in
  if Util.is_synthetic_name name then
    st.path
  else (
    if st.path = "" then name
    else st.path ^ sep ^ name
  )

let lookup_local st name =
  Hashtbl.find st.s_table name


