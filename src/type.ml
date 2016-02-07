open Typestack

type ty_info = {
  mutable file: string;
  mutable mutated: bool;
  mutable table: state option;
}
and bool_value =
  | True
  | False
  | Undecided
and ty =
  | Bool_ty of bool_value * state option * state option
  | Int_ty
  | Str_ty of string
  | Float_ty
  | Instance_ty of type_t
and
  type_t = {
  mutable info: ty_info;
  mutable ty: ty;
}
and
  state_type =
    Class
  | Instance
  | Function
  | Module
  | Global
  | Scope
and
state = {
  mutable parent: state option;
  mutable supers: state option;
  mutable s_type: state_type;
  mutable t_type: type_t option;
  mutable s_table: (string, binding list) Hashtbl.t;
  mutable path: string;
}
and
  kind =
  | ModuleK
  | ClassK
  | MethodK
  | ClassMethodK
  | AttributeK
  | ParameterK
  | ScopeK
  | VariableK
  | ConstK
and
  binding = {
  (* node: Node.node; *)
  qname: string;
  bfile: string;
  bty: type_t;
  kind: kind;
  start: int;
  tail: int;
  body_start: int;
  body_end: int;
}

let type_stack = TypeStack.empty;;

let is_mutated (t: type_t) = t.info.mutated

let set_mutated t m = t.info.mutated <- m

let set_file t f = t.info.file <- f

let set_table (t: type_t) (table: state) =
  t.info.table <- Some table

let is_undecided_bool t =
  match t.ty with
  | Bool_ty(bv, _, _) ->
    (match bv with Undecided -> true | _ -> false)
  | _ -> false

let is_num_type t =
  match t.ty with
  | Int_ty | Float_ty -> true
  | _ -> false

let is_str_type t =
  match t.ty with
  | Str_ty _ -> true
  | _ -> false

let is_unknow_type t =
  false (* fixme *)

let type_equal ty1 ty2 =
  match ty1.ty, ty2.ty with
  | Int_ty, Int_ty
  | Str_ty _, Str_ty _
  | Float_ty, Float_ty
  | Bool_ty _, Bool_ty _ -> true
  | _, _ -> false


let new_state ?(parent = None) state_ty : state =
  {
    parent = parent;
    supers = None;
    s_type = state_ty;
    t_type = None;
    s_table = Hashtbl.create 1;
    path = ""
  }

let new_ty_info() =
  { file = ""; mutated = false; table = None}

let new_bool_type ?(v = Undecided) ?(s1 = None) ?(s2 = None) () =
  { info = new_ty_info();
    ty = Bool_ty(v, s1, s2);
  }

let set_bool_value b v =
  match b.ty with
  | Bool_ty(_, s1, s2) -> {info = b.info; ty = Bool_ty(v, s1, s2)}
  | _ -> failwith "set_bool_value"

let set_bool_s1 b s1 =
  match b.ty with
  | Bool_ty(v, _, s2) -> b.ty <- Bool_ty(v, Some(s1), s2)
  | _ -> failwith "set_bool_s1"

let set_bool_s2 b s2 =
  match b.ty with
  | Bool_ty(v, s1, _) -> b.ty <- Bool_ty(v, s1, Some(s2))
  | _ -> failwith "set_bool_s2"

let bool_swap b =
  match b.ty with
  | Bool_ty(v, s1, s2) ->
    {b with ty = Bool_ty(v, s2, s1)}
  | _ -> failwith "bool_swap"


let set_state_parent st parent =
  st.parent <- Some(parent)

let set_state_stype st stype =
  st.s_type <- stype

let state_remove st id =
  Hashtbl.remove st.s_table id

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

let state_merge_with st1 st2 =
  st1

let state_merge st1 st2 =
  let ret = state_copy st1 in
  ignore(state_merge_with ret st2);
  ret

let state_update st id bindings =
  Hashtbl.add st.s_table id bindings

let state_update_bind st id binding =
  state_update st id [binding]

let is_global_name name =
  let open Core.Std in
  String.substr_index name "$" = Some(0)

