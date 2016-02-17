open Core.Std
open Node
open Type
open Binder

let rec transform (node:node) state =
  match node.ty with
  | Int(_) -> Type.int_ty
  | Float(_) -> Type.float_ty
  | Regexp(_, _) -> Type.str_ty
  | String(s) -> Type.new_str_type ~value:s ()
  | Symbol(s) -> Type.new_sym_type ~name:s ()
  | Void -> Type.cont_ty
  | StrEmbed(s) -> (
      ignore(transform s state);
      Type.str_ty
    )
  | UnaryOp(_, operand) -> transform operand state
  | Undef(elems) -> (
      List.iter elems ~f:(fun e -> ignore(transform e state));
      (* fixme *)
      Type.cont_ty
    )
  | Kwd(_, v) | Return(v) | Starred(v) | Yield(v)
    -> transform v state
  | Assign(t, rv) -> (
      let vt = transform rv state in
      Binder.bind_node state t vt;
      vt
    )
  | Block(nodes) -> (
      let return_ty = ref Type.cont_ty in
      List.iteri nodes ~f:(fun i n ->
          let ty = transform n state in
          if i = (List.length nodes) - 1 then return_ty := ty;
        );
      !return_ty
    )
  | _ -> Type.int_ty

let transform_expr node state =
  transform node state

