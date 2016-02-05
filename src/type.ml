open Typestack

type ty_info = {
  mutable file: string;
  mutable mutated: bool;
  mutable state: State.state;
}
and bool_value =
  | True
  | False
  | Undecided
and ty =
  | Bool_ty of bool_value
  | Int_ty
  | Str_ty of string
  | Float_ty
  | Instance_ty of type_t
and
  type_t = {
  info: ty_info;
  ty: ty;
}

let type_stack = TypeStack.empty;;

let is_mutated t = t.info.mutated

let set_mutated t m = t.info.mutated = m

let set_file t f = t.info.file = f

let is_undecided_bool t =
  match t.ty with
  | Bool_ty(bv) ->
    (match bv with Undecided -> true | _ -> false)
  | _ -> false

let is_num_type t =
  match t.ty with | Int_ty | Float_ty -> true
  | _ -> false


let is_str_type t =
  match t.ty with | Str_ty _ -> true
  | _ -> false


let equal ty1 ty2 =
  match ty1.ty, ty2.ty with
  | Int_ty, Int_ty
  | Str_ty _, Str_ty _
  | Float_ty, Float_ty
  | Bool_ty _, Bool_ty _ -> true
  | _, _ -> false
