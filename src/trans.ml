open Core.Std
open Node
open Type

let rec transform (node:Node.node) state =
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
  | _ -> Type.int_ty

let transform_expr node state =
  transform node state

