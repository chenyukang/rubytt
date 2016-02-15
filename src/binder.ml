open Node
open Type
open State

let rec bind state (target:node) (rt:type_t) kind =
  match target.ty with
  | Name _ -> bind_name state target rt kind
  | _ -> ()
and
  bind_node state (target:node) (rt:type_t) =
  let kind = match state.s_type with
    | State.Function -> Type.VariableK
    | State.Class | State.Instance -> Type.AttributeK
    | _ -> Type.ScopeK in
  bind state target rt kind

and
  bind_name state name (rt:type_t) kind =
  let id = name_node_id name in
  if Util.is_global_name id && (name_node_is_globalvar name) then (
    
  ) else ()

