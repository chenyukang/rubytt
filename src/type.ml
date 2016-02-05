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
  info: ty_info;
  ty: ty;
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
  parent: state option;
  supers: state option;
  s_type: state_type;
  t_type: type_t option;
  s_table: (string, binding list) Hashtbl.t;
  mutable path: string;
}
and
  kind =
  ModuleK
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
  node: Node.node;
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

let set_mutated t m = t.info.mutated = m

let set_file t f = t.info.file = f

let set_table (t: type_t) (table: state) =
  t.info.table = Some table

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

let equal ty1 ty2 =
  match ty1.ty, ty2.ty with
  | Int_ty, Int_ty
  | Str_ty _, Str_ty _
  | Float_ty, Float_ty
  | Bool_ty _, Bool_ty _ -> true
  | _, _ -> false


let new_state parent state_ty =
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

let new_bool_type v ?(s1 = None) ?(s2 = None)=
  { info = new_ty_info();
    ty = Bool_ty(v, s1, s2);
  }

let set_bool_value b v =
  match b.ty with
  | Bool_ty(_, s1, s2) -> b.ty = Bool_ty(v, s1, s2)
  | _ -> failwith "set_bool_value"

let set_bool_s1 b s1 =
  match b.ty with
  | Bool_ty(v, _, s2) -> b.ty = Bool_ty(v, s1, s2)
  | _ -> failwith "set_bool_s1"

let set_bool_s2 b s2 =
  match b.ty with
  | Bool_ty(v, s1, _) -> b.ty = Bool_ty(v, s1, s2)
  | _ -> failwith "set_bool_s2"

let bool_swap b =
  match b.ty with
  | Bool_ty(v, s1, s2) -> { info = b.info; ty = Bool_ty(v, s2, s2) }
  | _ -> failwith "bool_swap"
