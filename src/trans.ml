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

let rec state_lookup st name =
  match get_modulebinding_if_global st name with
  | None -> (
      match lookup_local st name with
      | Some(b) -> Some(b)
      | None -> (
          match st.parent with
          | Some(p) -> state_lookup p name
          | None -> None
        )
    )
  | Some(b) -> Some(b)

let rec bind state (target:node) rt kind =
  match target.ty with
  | Name _ -> bind_name state target rt kind
  | Array(elems) -> (
      (* a, b = 1, 2 *)
      bind_array state elems rt kind
    )
  | Subscript(value, slices) -> (
    )
  | _ -> ()

and
  bind_node state (target:node) rt =
  let kind = match state.s_type with
    | State.Function -> Type.VariableK
    | State.Class | State.Instance -> Type.AttributeK
    | _ -> Type.ScopeK in
  bind state target rt kind

and
  bind_name state name (rt:type_t) kind =
  let id = name_node_id name in
  Printf.printf "bind name: %s ty: %s\n" id (Type.type_to_str rt);
  if Util.is_global_name id && (name_node_is_globalvar name) then (
    let b = new_binding name rt kind in
    State.state_update_bind global_table id b;
    put_ref name b;
  ) else (
    state_insert state id name rt kind
  )

and
  bind_array state elems rt kind =
  let elems_size = List.length elems in
  match rt.ty with
  | List_ty(_, tys, _) -> (
      let ty_size = List.length tys in
      if ty_size <> elems_size then
        Printf.printf "error array assign size mismatch: %d %d\n" elems_size ty_size
      else
        List.iteri elems ~f:(fun i e ->
            let ty = List.nth_exn tys i in
            bind state e ty kind;
          )
    )
  | _ -> (Printf.printf "error array assign size mismtach: %d 0\n" elems_size);

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
      ignore(transform s state); Type.str_ty
    )
  | Name(id, _) -> (
      match state_lookup state id with
      | Some(bs) -> (
          put_refs node bs;
          make_unions bs
        )
      | _ when id = "true" || id = "false" -> (
          Type.bool_ty
        )
      | _ -> ( Printf.printf "error: unbound variable for %s\n" id;
               Type.unkown_ty)
    )
  | BinOp(_, ln, rn) -> (
      let lt = transform ln state in
      let rt = transform rn state in
      if not (type_equal lt Type.unkown_ty) then lt
      else (
        if not (type_equal rt Type.unkown_ty) then rt
        else Type.unkown_ty
      )
    )
  | Array(elems) -> (
      let list_ty = new_list_type() in
      List.iter elems ~f:(fun e ->
          let t = transform e state in
          list_ty_add list_ty t;
        );
      list_ty
    )
  | UnaryOp(_, operand) -> transform operand state
  | Undef(elems) -> (
      List.iter elems ~f:(fun e -> ignore(transform e state));
      (* Fixme *)
      Type.cont_ty
    )

  | Assign(t, rv) -> (
      let vt = transform rv state in
      bind_node state t vt;
      vt
    )
  | Subscript(value, slices) -> (
      let vt = transform value state in
      let st = List.map slices ~f:(fun i -> transform i state) in
      Type.get_subscript_ty vt st
    )
  | Block(nodes) -> (
      let return_ty = ref Type.cont_ty in
      List.iteri nodes ~f:(fun i n ->
          let ty = transform n state in
          if i = (List.length nodes) - 1 then return_ty := ty;
        );
      !return_ty
    )
  (* | Raise() *)
  | Kwd(_, v) | Return(v) | Starred(v) | Yield(v)
    -> transform v state
  | _ -> Type.int_ty

let transform_expr node state =
  transform node state
