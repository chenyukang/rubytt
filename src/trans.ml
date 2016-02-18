open Core.Std
open Node
open Type
open State

let global_refs: (Node.node, Type.binding_ty list) Hashtbl.t = Hashtbl.Poly.create()

let state_insert st id node ty kind =
  let b = new_binding node ty kind in
  State.state_update_bind st id b

let put_refs node bs =
  let binded = Hashtbl.find global_refs node in
  match binded with
  | None ->  (
      Hashtbl.add_exn global_refs ~key:node ~data: bs
    )
  | Some(v) -> (
      List.iter bs ~f:(fun b -> Type.binding_add_ref b node);
      ignore(Hashtbl.replace global_refs ~key:node ~data:(v @ bs));
    )

let put_ref node bind =
  let bs = [bind] in
  put_refs node bs


let get_modulebinding_if_global st name =
  let res = ref None in
  if Util.is_global_name name then
    if global_table <> st then
        res := State.lookup_local global_table name;
  !res


let rec bind state (target:node) (rt:type_t) kind =
  match target.ty with
  | Name _ -> bind_name state target rt kind
  | Array(elems) -> ( failwith "array as target!")
  | Subscript(value, slices) -> (
      (* let val_ty =  *)
    )
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
  Printf.printf "name: %s\n" id;
  if Util.is_global_name id && (name_node_is_globalvar name) then (
    let b = new_binding name rt kind in
    State.state_update_bind global_table id b;
    put_ref name b;
  ) else (
    state_insert state id name rt kind
  )

and
  transform (node:node) state =
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
      bind_node state t vt;
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
