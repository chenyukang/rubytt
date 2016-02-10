open Node
open Type

let rec transform (node:Node.node) state =
  match node.ty with
  | Int(_) -> Type.int_ty
  | String(s) -> Type.new_str_type ~value:s ()
  | Symbol(s) -> Type.new_sym_type ~name:s ()
  | Void -> Type.cont_ty
  | Return(v) -> transform v state
  | _ -> Type.int_ty

let transform_expr node state =
  transform node state

