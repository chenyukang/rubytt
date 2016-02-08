open Typestack
open State
open Binding

type bind_ty = (type_t, Node.node) binding
and
  state_t = (type_t, bind_ty) state
and
  ty_info = {
  mutable file: string;
  mutable mutated: bool;
  mutable table:  state_t option;
}
and
  bool_value =
  | True
  | False
  | Undecided
and ty =
  | Bool_ty of bool_value * state_t option * state_t option
  | Int_ty
  | Str_ty of string
  | Float_ty
  | Instance_ty of type_t
  | ClassType of string * type_t option * type_t option
and
  type_t = {
  mutable info: ty_info;
  mutable ty: ty;
}

let type_stack = TypeStack.empty;;

let is_mutated (t: type_t) = t.info.mutated

let set_mutated t m = t.info.mutated <- m

let set_file t f = t.info.file <- f

let set_table (t: type_t) (table: state_t) =
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
  | ClassType _, ClassType _ -> ty1 = ty2
  | _, _ -> false

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


let new_class_type name parent ?(super = None) =
  let ret = { info = new_ty_info();
              ty = ClassType(name, None, None);
            } in
  set_table ret (State.new_state ~parent:(Some parent) State.Class);
  (match ret.info.table with
  | Some(s) -> State.set_state_ttype s (Some ret);
  | _ -> failwith "new_class_type error table");
  ret

let classty_set_name c name =
  match c.ty with
  | ClassType(_name, canon, super) -> c.ty <- ClassType(name, canon, super)
  | _ -> failwith "classty_set_name"

let classty_set_canon c canon =
  match c.ty with
  | ClassType(name, _canon, super) -> c.ty <- ClassType(name, canon, super)
  | _ -> failwith "classty_set_canon"


let classty_add_super c super = ()
