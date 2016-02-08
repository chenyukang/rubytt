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
  (* name * instance_type * superclass *)
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

let bool_set_value b v =
  match b.ty with
  | Bool_ty(_, s1, s2) -> {info = b.info; ty = Bool_ty(v, s1, s2)}
  | _ -> failwith "bool_set_value"

let bool_set_s1 b s1 =
  match b.ty with
  | Bool_ty(v, _, s2) -> b.ty <- Bool_ty(v, Some(s1), s2)
  | _ -> failwith "bool_set_s1"

let bool_set_s2 b s2 =
  match b.ty with
  | Bool_ty(v, s1, _) -> b.ty <- Bool_ty(v, s1, Some(s2))
  | _ -> failwith "bool_set_s2"

let bool_swap b =
  match b.ty with
  | Bool_ty(v, s1, s2) ->
    {b with ty = Bool_ty(v, s2, s1)}
  | _ -> failwith "bool_swap"


let classty_set_name c name =
  match c.ty with
  | ClassType(_name, canon, super) -> c.ty <- ClassType(name, canon, super)
  | _ -> failwith "classty_set_name"

let classty_set_canon c canon =
  match c.ty with
  | ClassType(name, _canon, super) -> c.ty <- ClassType(name, canon, super)
  | _ -> failwith "classty_set_canon"

let classty_add_super c super =
  match c.ty with
  | ClassType(name, canon, _) -> c.ty <- ClassType(name, canon, super)
  | _ -> failwith "classty_add_super"

let new_class_type name parent ?(super = None) =
  let ret = { info = new_ty_info();
              ty = ClassType(name, None, None);
            } in
  let state = (State.new_state ~parent:parent State.Class) in
  set_table ret state;
  State.set_state_ttype state (Some ret);
  (match parent with
   | Some(p) -> State.set_path state (State.extend_path p name "::")
   | _ -> State.set_path state name);
  (match super with
   | Some(s) -> (
       classty_add_super ret super;
       State.set_supers state s.info.table (* Fixme *)
     )
   | _ -> ());
  ret
