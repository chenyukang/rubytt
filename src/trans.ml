open Core.Std
open Node
open Type
open Global
open State

let new_bind node ty kind =
  let bind = Type.new_binding node ty kind in
  Global.register_bind bind;
  bind

let state_add_mode = ref 0;; (* fixme *)
let state_insert st id node ty kind =
  let b = new_bind node ty kind in
  let qname = match ty.ty with
  | Module_ty(_) -> "" (* fixme *)
  | Fun_ty(_) -> extend_path st id "#"
  | _ -> extend_path st id "::"
  in
  set_bind_qname b qname;
  (* Printf.printf "set qname: %s now: %d\n" qname !state_add_mode; *)
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
    if not (phys_equal global_table st) then
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

let lookup_ty state id =
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
  if Util.is_global_name id && (name_node_is_globalvar name) then (
    let b = new_bind name rt kind in
    (* Printf.printf "bind global name: %s ty: %s\n" id (Printer.type_to_str rt 0); *)
    State.state_update_bind global_table id b;
    put_ref name b;
  ) else (
    (* Printf.printf "bind local name: %s ty: %s\n" id (Printer.type_to_str rt 0); *)
    state_insert state id name rt kind
  )

and
  bind_array state elems rt kind =
  let elems_size = List.length elems in
  match rt.ty with
  | List_ty(_, tys, _) -> (
      let ty_size = List.length tys in
      if ty_size <> elems_size then (
        (* Printf.printf "error array assign size mismatch: %d %d\n" elems_size ty_size *)
      )
      else
        List.iteri elems ~f:(fun i e ->
            let ty = List.nth_exn tys i in
            bind state e ty kind
          )
    )
  | _ -> (* (Printf.printf "error array assign size mismtach: %d 0\n" elems_size) *) ()

and
  transform (node:node_t) state =
  (* Printf.printf "trans: %s\n" (Printer.node_to_str node 0); *)
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
      (* Printf.printf "lookup name: %s\n" id; *)
      match state_lookup state id with
      | Some(bs) -> (
          (* Printf.printf "put ref name: %s\n" id; *)
          Global.put_refs node bs;
          Global.set_resolve node;
          (* List.iter bs ~f:(fun t -> Printf.printf "now: %s\n" (Printer.type_to_str t.bind_ty 0)); *)
          make_unions_from_bs bs
        )
      | _ when id = "true" || id = "false" -> (
          Type.bool_ty
        )
      | _ -> (
          (* Printf.printf "error: unbound variable for %s\n" id; *)
          Global.set_unresolve node;
          Type.unkown_ty
      )
    )
  | BinOp(op, ln, rn) -> (
    let str = Printer.node_to_str node 0 in
    (* Printf.printf "now result: %s\n" str; *)
      let _ = transform ln state in
      let rt = transform rn state in
      if Node.is_logic_bin node then Type.bool_ty
      else (
        (* Printf.printf "rt type: %s\n" (Printer.type_to_str rt 0); *)
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
  | UnaryOp(_, operand) -> (
    let r = transform operand state in
    if Node.is_logic_bin node then Type.bool_ty else r
  )
  | Assign(target, rvalue) -> (
    let vt = transform rvalue state in
    let _  = transform target state in
    if Node.is_instance_var target then (
        let this_ty = lookup_ty state "self" in
        if not(Type.is_unkown_ty this_ty) then
          bind_node this_ty.info.table target vt
    ) else (
      let target_v = Printer.node_to_str target 0 in
      let type_v = Printer.type_to_str vt 0 in
      (* Printf.printf "%s -> %s\n" target_v type_v; *)
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
        | Some(_bs) -> (
            Global.put_refs attr _bs;
            make_unions_from_bs _bs
          )
        | _ -> (
          (* Printf.printf "error: '%s' attribute not found for : %s\n" *)
          (*               id (Printer.type_to_str target_ty 0); *)
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
  | If(test, body, _else) -> (
      let _ = transform test state in
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
          _state := loc_ty.info.table
      );
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
      let func_str = Printer.node_to_str func 0 in
      (* Printf.printf "func_str: %s\n" func_str; *)
      if is_attr func then (
        _func := attr_target func;
        (* Printf.printf "attr target: %s\n" (Printer.node_to_str !_func 0); *)
        _name := attr_attr func;
        (* Printf.printf "attr name: %s\n" (Printer.node_to_str !_name 0) *)
      );
      let fun_ty = transform !_func state in
      (* Printf.printf "type str: %s\n" (Printer.type_to_str fun_ty 0); *)
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
        (* FIXME *)
      );
      let id = name_node_id name in
      let parent = Some(state) in
      let super_ty = transform super state in
      let class_ty = new_class_type id parent ~super:(Some super_ty) () in
      bind state name class_ty Type.ClassK;
      state_insert class_ty.info.table "self" name class_ty Type.ScopeK;
      add_inst_type_from_db class_ty;
      ignore(transform body class_ty.info.table);
      Type.cont_ty
    )
  | Kwd(_, v) | Return(v) | Starred(v) | Yield(v)
    -> transform v state
  | _ -> Type.unkown_ty

and resolve_call obj name args_ty star_ty block_arg_ty call state =
  match obj.ty with
  | Int_ty | Str_ty _ | Float_ty -> obj
  | Fun_ty(_) -> apply_func obj args_ty star_ty block_arg_ty call
  | Class_ty(_) -> (
    let id = name_node_id name in
    match id with
      | "new" | "create" -> (
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
          (* Printf.printf "error method name: %s\n" id; *)
        (* ignore(failwith "resolve_call"); *)
        transform name obj.info.table
      )
  )
  | Instance_ty(class_ty) -> (
      if not (Type.is_unkown_ty obj) then (
        let id = name_node_id name in
        let method_ty = lookup_attr_ty class_ty.info.table id in
        if Type.is_unkown_ty method_ty then (
          (* Printf.printf "error unkown method: %s\n" id; *)
          unkown_ty
        ) else
          apply_func method_ty args_ty star_ty block_arg_ty call
      ) else unkown_ty
  )
  | _ -> (
    match name_node_id name with
    | "to_s" -> Type.new_str_type()
    | "to_sym" -> Type.new_sym_type()
    | "to_i" -> Type.new_int_type()
    | _ -> (
      (* (Printf.printf "try to resolve_call: unkown_ty\n"); *)
      Type.unkown_ty
    ))

and bind_param_tys env args args_types =
  List.iteri args ~f:(fun i arg ->
      if i < List.length args_types then (
        let arg_ty = List.nth_exn args_types i in
        bind env arg arg_ty Type.ParameterK
      ) else
        bind env arg unkown_ty Type.ParameterK
    )

and apply_func fun_ty args_ty star_ty block_arg_ty call =
  (* Printf.printf "apply_func: \n" ; *)
  Global.set_called fun_ty;
  if not (Global.contains_call call) && (Type.is_fun_ty fun_ty) then (
    let info = Type.fun_ty_info fun_ty in
    let node_info = func_node_info info.fun_node in
    let env = State.new_state ~parent:info.env State.Function in
    let _ = Global.push_call call in
    let _ = bind_param_tys env node_info.args args_ty in
    let ret_ty = transform node_info.body env in
    if not (Type.type_equal ret_ty fun_ty) then (
      fun_ty_set_ret_ty fun_ty ret_ty; ret_ty)
    else Type.unkown_ty)
  else
    Type.unkown_ty

(* add types which analyzed from scheme.rb *)
and add_inst_type_from_db class_ty =
  let class_name = classty_get_name class_ty in
  let table = class_ty.info.table in
  let db_name = Db.match_db_for_model class_name in
  let columns = match Hashtbl.find Db.tables db_name with
    | Some(cols) -> cols
    | _ -> [] in
  List.iter columns ~f:(fun col ->
                        let name = col.(0) in
                        let ty_str = col.(1) in
                        (* Printf.printf "name: %s type: %s\n" name ty_str; *)
                        let ty = match ty_str with
                          | "string" | "text"  -> new_str_type()
                          | "boolean" -> new_bool_type()
                          | "integer" -> new_int_type()
                          | _ -> unkown_ty in
                        state_insert table name nil_node ty Type.ScopeK;
                       );
  ()

let apply_uncalled () =
  TypeSet.Set.iter !Global.uncalled ~f:(fun fun_ty ->
      let info = Type.fun_ty_info fun_ty in
      let node_info = func_node_info info.fun_node in
      let id = name_node_id node_info.name in
      (* Printf.printf "apply id: %s\n" id; *)
      (* let env = State.new_state ~parent:info.env State.Function in *)
      (* let ret_ty = transform node_info.body env in *)
      (* ignore(fun_ty_set_ret_ty fun_ty ret_ty)) *)
      let _ = apply_func fun_ty [] unkown_ty unkown_ty info.fun_node in
      ())

let transform_expr node state =
  ignore(transform node state);
  apply_uncalled()
