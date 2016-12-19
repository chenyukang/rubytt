open Core.Std
open Node

type env = {
  env_ty : string;
  mutable parent: env option;
  mutable visited: (string, bool) Hashtbl.t;
  mutable variables: (Node.node_t, bool) Hashtbl.t;
  mutable children: env list;
}

let make_env ?ty:(ty="top") ()=
  {
    env_ty = ty;
    parent = None;
    visited = Hashtbl.Poly.create();
    variables = Hashtbl.Poly.create();
    children = [];
  }

let root_env = ref (make_env());;

let new_child ?ty:(ty="module") parent =
  let child = (make_env ~ty:ty ()) in
  parent.children <- parent.children @ [child];
  child.parent <- Some(parent);
  child

let clear () =
  root_env := make_env()

let add_variable env var =
  if is_instance_var var then (
    let cur = ref env in
    let finished = ref false in
    while !finished = false && !cur.env_ty <> "module" do
      match !cur.parent with
      | Some(p) -> cur := p
      | None -> finished := true
    done;
    if !cur.env_ty = "module" then
      ignore(Hashtbl.add !cur.variables ~key:var ~data:true)
  )
  else
    ignore(Hashtbl.add env.variables ~key:var ~data:true)

let visited_variable env name =
  ignore(Hashtbl.add env.visited ~key:name ~data:true)

let line_no_from_file file node =
  let ss = node.info.ss in
  let buf = Util.read_file_to_str file in
  let pos = ref 0 in
  let num = ref 0 in
  let lines = String.split buf ~on:'\n' in
  while !num < (List.length lines) && !pos <= ss do
    let line = List.nth_exn lines !num in
    pos := !pos + (String.length line) + 1;
    incr num
  done;
  !num

let node_to_info node =
  let cur_dir = Sys.getcwd() in
  ((Stringext.replace_all node.info.file ~pattern:cur_dir ~with_:"."),
   (line_no_from_file node.info.file node), (name_node_id node))

let rec env_info ?check_global:(check_global=false) ?check_inst:(check_inst=false) env =
  let res = ref [] in
  let rec env_visited var env =
    let name = name_node_id var in
    (* Printf.printf "check now: %s\n" name; *)
    ((Hashtbl.find env.visited name) <> None) ||
    (List.find env.children
       ~f:(fun e ->
           match var.ty with
           | Name(_, Global) | Name(_, Instance) -> env_visited var e
           | _ -> (e.env_ty <> "func") && (env_visited var e)
         )
     <> None) in
  Hashtbl.iter env.variables
    ~f:(fun ~key:v ~data:_ ->
        if (match v.ty with
            | Name(_, Global) -> check_global && (not (env_visited v env))
            | Name(_, Instance) -> check_inst && (not (env_visited v env))
            | Name(_, Local) -> not (env_visited v env)
            | _ -> false) then
          (* add unvisited variable *)
          res := !res @ [node_to_info v]
       );

  List.iter env.children
            ~f:(fun e -> res := !res @ (env_info ~check_global:check_global ~check_inst:check_inst e));
  !res


let rec env_defname_info env =
  let res = ref [] in
  let rec env_find_def name env =
    let found = ref false in
    Hashtbl.iter env.variables
                 ~f:(fun ~key:v ~data:_ ->
                     if (match v.ty with
                         | Name(s, _) -> (name_node_id v) = name
                         | _ -> false ) then
                       found := true);
    if !found = false then
      match env.parent with
      | Some(parent) -> env_find_def name parent
      | _ -> false
    else
      true in
  Hashtbl.iter env.visited ~f:(fun ~key:name ~data:_ ->
                            if env_find_def name env = false then
                              res := !res @ [name]);
  List.iter env.children ~f:(fun e -> res := !res @ (env_defname_info e));
  !res

let sort_result res =
  if List.length res = 0 then
    "\nNo unsed variable issue found, ^_^\n"
  else
    let infos = List.sort res ~cmp:(fun (f1, l1, v1) (f2, l2, v2) ->
        let r = String.compare f1 f2 in
        if r <> 0 then r else (
          if l1 <> l2 then (l1 - l2) else String.compare v1 v2
        )
      )
    in
    List.fold infos  ~init:"" ~f:(fun acc (f, l, v) ->
        acc ^ "\n" ^ (Printf.sprintf "unvisited variable %s(%d) : %s" f l v))

let env_unused check_global check_inst =
  let unused_result = env_info
                        !root_env
                        ~check_global:check_global
                        ~check_inst:check_inst in
  sort_result unused_result

let check_unused asts =
  let rec iter ast env =
    let _iter ast = iter ast env in
    (* let str = Printer.node_to_str ast 0 in *)
    (* Printf.printf "%s\n\n" str; *)
    let try_add_variable env v =
      match v.ty with
      | Name(s, t) -> (
        (* Printf.printf "set variable: %s\n" (name_node_id v); *)
        if String.get s 0 <> '_' then (
          match t with
          | Local | Global | Instance -> add_variable env v
          | _ -> ()
        )
      )
      | _ -> () in
    match ast.ty with
    | Name(id, _) -> (
        let name = name_node_id ast in
        (* Printf.printf "lookup: %s\n" name; *)
        visited_variable env name
      )
    | Assign(target, value) -> (
      let _ = match target.ty, value.ty with
        | Name(_, _), _ -> try_add_variable env target
        | Array(ls), Array(_) -> (
            List.iter ls ~f:(fun e -> try_add_variable env e)
          )
        | _, _ -> (_iter target) in _iter value
    )
    | Func(info) -> (
        let new_env = if (is_lambda ast) then env
                      else (new_child ~ty:"func" env) in
        List.iter info.args ~f:(fun arg ->
                                match arg.ty with
                                | Name(_, _) -> try_add_variable new_env arg
                                | _ -> ()
                               );
        iter info.body new_env
      )
    | Class(_, _, body, _, _) | Module(_, _, body, _) ->  iter body (new_child env)
    | Block(stmts) -> List.iter stmts ~f:_iter
    | Undef(nodes) -> List.iter nodes ~f:_iter
    | BinOp(_, ln, rn) -> List.iter [ln; rn] ~f:_iter
    | UnaryOp(_, operand) -> _iter operand
    | Attribute(target, _) -> _iter target
    | StrEmbed(value) -> _iter value
    | Handler(_, _, handler, _else) -> List.iter [handler; _else] ~f:_iter
    | For(target, it, body) -> List.iter [target; it; body] ~f:_iter
    | If(test, body, _else) -> List.iter [test; body; _else] ~f:_iter
    | While(test, body) -> List.iter [test; body] ~f:_iter
    | Subscript(value, slices) -> List.iter ([value] @ slices) ~f:_iter
    | Try(body, rescue, _else, final) -> List.iter [body; rescue; _else; final] ~f:_iter
    | Call(func, pos, star, block_arg) -> List.iter (([func] @ [star] @ pos) @ [block_arg]) ~f:_iter
    | Array(elms) -> List.iter elms ~f:_iter
    | Dict(ks, vs) -> List.iter (ks @ vs) ~f:_iter
    | Kwd(_, v) | Return(v) | Starred(v) | Yield(v) -> _iter v
    | _ -> () in
  List.iter asts ~f:(fun ast -> iter ast !root_env);
  let res = env_unused false false in
  Printf.printf "%s\n" res
