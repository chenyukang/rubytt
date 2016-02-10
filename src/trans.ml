open Node
open Type

let rec transform (node:Node.node) state =
  match node.ty with
  | Int(_) -> Type.Int_ty
  | String(s) -> Type.Str_ty(s)
  | _ -> Type.Int_ty

let transform_expr node state =
  transform node state

