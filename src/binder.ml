open Node
open Type
open State

let state_insert st id node ty kind =
  let b = new_binding node ty kind in
  state_update_bind st id b

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
    let b = new_binding name rt kind in
    state_update_bind global_table id b;
  ) else (
    state_insert state id name rt kind
  )

