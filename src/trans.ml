open Core.Std
open Node
open Type
open Global

let state_add_mode = ref 0;;
let state_insert st id node ty kind =
  let b = new_binding node ty kind in
  if !state_add_mode = 0 then
    State.state_update_bind st id b
  else
    State.state_add_bind st id b

let put_ref node bind =
  let bs = [bind] in
  Global.put_refs node bs

let get_modulebinding_if_global st name =
  let res = ref None in
  if Util.is_global_name name then
    if not(phys_equal global_table st) then
        res := State.lookup_local global_table name;
  !res

let rec state_lookup st name =
  match get_modulebinding_if_global st name with
  | None -> (
      match State.lookup_local st name with
      | Some(b) -> Some(b)
      | None -> (
          match State.parent st with
          | Some(p) -> state_lookup p name
          | None -> None
        )
    )
  | Some(b) -> Some(b)

let looked = Hashtbl.Poly.create();;
let rec lookup_attr state id =
  match Hashtbl.find looked state with
  | Some(_) -> None
  | _ -> (
      let bs = State.lookup_local state id in
      match bs with
      | Some(_) -> bs
      | _ -> (
          Hashtbl.add_exn looked ~key:state ~data:true;
          let res = match State.parent state with
          | Some(p) -> lookup_attr p id
          | _ -> None in
          Hashtbl.remove looked state;
          res)
    )

let rec lookup_attr_ty state id =
  let bs = lookup_attr state id in
  match bs with
  | Some(_bs) -> make_unions_from_bs _bs
  | _ -> (
      match State.super state with
      | Some(super) -> lookup_attr_ty super id
      | _ -> unkown_ty)

let rec lookup_ty state id =
  let bs = state_lookup state id in
  match bs with
  | Some(_bs) -> make_unions_from_bs _bs
  | _ -> unkown_ty

let lookup_attr_tagged st attr tag =
  lookup_attr st (Util.make_tag_id attr tag)

let rec bind state (target:node_t) rt kind =
  match target.ty with
  | Name _ -> bind_name state target rt kind
  | Array(elems) -> (
      (* a, b = 1, 2 *)
      bind_array state elems rt kind
    )
  | Subscript(_, _) -> (
    )
  | _ -> ()

and lookup_or_create_module state locator file =
  let existing = transform locator state in
  let id = name_node_id locator in
  match existing.ty with
  | Module_ty _ -> existing
  | _ when is_name locator -> (
      let bindings = lookup_attr state id in
      let ret = ref Type.cont_ty in
      (match bindings with
        | Some(bs) -> (
            if List.length bs > 0 && (List.nth_exn bs 0).kind = Type.ModuleK then
              ret := (List.nth_exn bs 0).bind_ty
          )
        | None -> ());
      if (type_equal !ret Type.cont_ty) then (
        ret := new_module_type id file (Some state);
      );
      !ret
    )
  | _ when is_attr locator -> (
      Type.cont_ty
    )
  | _ -> (
      new_module_type id "" (Some state)
    )

and
  bind_node state (target:node_t) rt =
  let kind = match State.s_type state with
    | State.Function -> Type.VariableK
    | State.Class | State.Instance -> Type.AttributeK
    | _ -> Type.ScopeK in
  bind state target rt kind

and
  bind_name state name (rt:type_t) kind =
  let id = name_node_id name in
  (* Printf.printf "bind name: %s ty: %s\n" id (Printer.type_to_str rt 0); *)
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
      if ty_size <> elems_size then (
        Printf.printf "error array assign size mismatch: %d %d\n" elems_size ty_size
      )
      else
        List.iteri elems ~f:(fun i e ->
            let ty = List.nth_exn tys i in
            bind state e ty kind
          )
    )
  | _ -> (Printf.printf "error array assign size mismtach: %d 0\n" elems_size)

and
  transform (node:node_t) state =
  match node.ty with
  | Nil -> Type.unkown_ty
  | Int(_) -> Type.int_ty
  | Float(_) -> Type.float_ty
  | Regexp(_, _) -> Type.str_ty
  | String(s) -> Type.new_str_type ~value:s ()
  | Symbol(s) -> Type.new_sym_type ~name:s ()
  | Void -> Type.cont_ty
  | StrEmbed(s) -> (
      ignore(transform s state); Type.str_ty
    )
  | Undef(nodes) -> (
      List.iter nodes ~f:(fun n ->
          let _ = transform n state in
          match n.ty with
          | Name(id, _) -> (State.remove state id)
          | _ -> ()
        );
        Type.cont_ty
    )
  | Name(id, _) -> (
      Printf.printf "lookup name: %s\n" id;
      match state_lookup state id with
      | Some(bs) -> (
          Global.put_refs node bs;
          Global.set_resolve node;
          make_unions_from_bs bs
        )
      | _ when id = "true" || id = "false" -> (
          Type.bool_ty
        )
      | _ -> ( Printf.printf "error: unbound variable for %s\n" id;
               Global.set_unresolve node;
               Type.unkown_ty)
    )
  | BinOp(_, ln, rn) -> (
      let lt = transform ln state in
      let rt = transform rn state in
      if not (type_equal rt Type.unkown_ty) then rt
      else Type.unkown_ty
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
  | Assign(target, rvalue) -> (
      let vt = transform rvalue state in
      if Node.is_instance_var target then (
        let this_ty = lookup_ty state "self" in
        if not(Type.is_unkown_ty this_ty) then
          bind_node this_ty.info.table target vt
      ) else (
        bind_node state target vt
      );
      vt
    )
  | Attribute(target, attr) -> (
      if is_nil target then transform attr state
      else (
        let target_ty = transform target state in
        let id = name_node_id attr in
        let bs = lookup_attr target_ty.info.table id in
        match bs with
        | Some(_bs) -> make_unions_from_bs _bs
        | _ -> (
            Printf.printf "error: '%s' attribute not found for : %s\n"
              id (Printer.type_to_str target_ty 0);
            Type.unkown_ty
          )
      )
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
  | Handler(_, _, handler, _else) -> (
      (* Fixme *)
      let handle_ty = transform handler state in
      let else_ty = transform _else state in
      make_unions [handle_ty; else_ty]
    )
  (* | Raise() *)
  | Control(_) -> Type.cont_ty
  | For(_, _, body) -> transform body state
  | If(_, body, _else) -> (
      let body_ty = transform body state in
      incr state_add_mode;
      let else_ty = transform _else state in
      decr state_add_mode;
      make_unions [body_ty; else_ty]
    )
  | While(test, body) -> (
      let _ = transform test state in
      transform body state
    )
  | Try(body, rescue, _else, final) -> (
      let rescue_ty = transform rescue state in
      let body_ty = transform body state in
      let else_ty = transform _else state in
      let final_ty = transform final state in
      make_unions [body_ty; else_ty; rescue_ty; final_ty]
    )
  | Func(info) -> (
      let _state = ref state in (
      if is_attr info.locator then
        let loc_ty = transform (attr_target info.locator) state in
        if not (type_equal loc_ty unkown_ty) then
          _state := loc_ty.info.table);
      let func_ty = new_fun_ty node (Some !_state) in
      let args_ty = List.map info.defaults ~f:(fun arg -> transform arg !_state) in
      let state_ty = ty_of_state !_state in
      if (not info.is_lambda) && (is_class_ty state_ty) then
        fun_ty_set_class_ty func_ty (Some state_ty);
      fun_ty_set_def_tys func_ty args_ty;
      bind_name !_state info.name func_ty Type.MethodK;
      State.set_parent func_ty.info.table !_state;
      let id = name_node_id info.name in
      State.set_path func_ty.info.table (State.extend_path !_state id "#");
      Global.set_uncalled func_ty;
      func_ty
    )
  | Call(func, pos, star, block_arg) -> (
      let _func = ref func in
      let _name = ref nil_node in
      if is_attr func then (
        _func := attr_target func;
        _name := attr_attr func
      );
      let fun_ty = transform !_func state in
      let args_ty = List.map pos ~f:(fun x -> transform x state) in
      let star_ty = transform star state in
      let block_arg_ty = transform block_arg state in
      resolve_call fun_ty !_name args_ty star_ty block_arg_ty node state
    )
  | Module(locator, name, body, _) -> (
      let module_ty = lookup_or_create_module state locator node.info.file in
      bind state name module_ty Type.ModuleK;
      state_insert module_ty.info.table "self" name module_ty Type.ScopeK;
      ignore(transform body module_ty.info.table);
      module_ty
    )
  | Class(name, super, body, _, static) -> (
      if (is_nil name) = false && static then (

      );
      let id = name_node_id name in
      let parent = Some(state) in
      let super_ty = transform super state in
      let class_ty = new_class_type id parent ~super:(Some super_ty) () in
      bind state name class_ty Type.ClassK;
      state_insert class_ty.info.table "self" name class_ty Type.ScopeK;
      ignore(transform body class_ty.info.table);
      Type.cont_ty
    )
  | Kwd(_, v) | Return(v) | Starred(v) | Yield(v)
    -> transform v state
  | _ -> Type.unkown_ty

and resolve_call obj name args_ty star_ty block_arg_ty call state =
  match obj.ty with
  | Fun_ty(_) -> apply_func obj args_ty star_ty block_arg_ty call
  | Class_ty(_) -> (
      let id = name_node_id name in
      match id with
      | "new" -> (
          (* class contructor *)
          let class_ty = obj in
          let inst_ty = new_instance_type class_ty in
          let inst_state = inst_ty.info.table in
          let init_func_ty = lookup_attr_ty inst_state "initialize" in
          classty_set_canon class_ty (Some inst_ty);
          if not (type_equal init_func_ty unkown_ty) then (
            let bs = lookup_attr inst_state "initialize" in
            (match bs with
             | Some(_bs) -> Global.put_refs name _bs
             | _ -> ());
            fun_ty_set_self_ty init_func_ty (Some inst_ty);
            ignore(apply_func init_func_ty args_ty star_ty block_arg_ty call);
          );
          inst_ty
        )
      | _ -> (
          Printf.printf "error method name: %s\n" id;
          (* ignore(failwith "resolve_call"); *)
          unkown_ty
        )
    )
  | Instance_ty(class_ty) -> (
      if not (Type.is_unkown_ty obj) then (
        let id = name_node_id name in
        let method_ty = lookup_attr_ty class_ty.info.table id in
        if Type.is_unkown_ty method_ty then (
          Printf.printf "error unkown method: %s\n" id;
          unkown_ty
        ) else
          apply_func method_ty args_ty star_ty block_arg_ty call
      ) else unkown_ty
    )
  | _ -> (Printf.printf "try to resolve_call: unkown_ty\n"); Type.unkown_ty

and bind_param_tys env args args_types =
  List.iteri args ~f:(fun i arg ->
      if i < List.length args_types then (
        let arg_ty = List.nth_exn args_types i in
        bind env arg arg_ty Type.ParameterK
      )
    )

and apply_func fun_ty args_ty star_ty block_arg_ty call =
  Global.set_called fun_ty;
  if not (is_nil call) && (Global.contains_call call) then
    Type.unkown_ty
  else (
    let info = Type.fun_ty_info fun_ty in
    let node_info = func_node_info info.fun_node in
    let env = State.new_state ~parent:info.env State.Function in
    let _ = Global.push_call call in
    let _ = bind_param_tys env node_info.args args_ty in
    let ret_ty = transform node_info.body env in
    fun_ty_set_ret_ty fun_ty ret_ty;
    ret_ty)

let apply_uncalled () =
  TypeSet.Set.iter !Global.uncalled (fun fun_ty ->
      let info = Type.fun_ty_info fun_ty in
      let node_info = func_node_info info.fun_node in
      let id = name_node_id node_info.name in
      (* Printf.printf "apply id: %s\n" id; *)
      let env = State.new_state ~parent:info.env State.Function in
      let ret_ty = transform node_info.body env in
      ignore(fun_ty_set_ret_ty fun_ty ret_ty))

let transform_expr node state =
  ignore(transform node state);
  apply_uncalled()
