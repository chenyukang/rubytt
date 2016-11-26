open Core.Std
open Node

type env = {
    mutable visited: (string, bool) Hashtbl.t;
    mutable variables: (Node.node_t, bool) Hashtbl.t;
    mutable children: env list;
  }

let make_env () = {
    visited = Hashtbl.Poly.create();
    variables = Hashtbl.Poly.create();
    children = [];
  }

let root_env = make_env();;

let new_child parent =
  Printf.printf "new child .........\n";
  let child = make_env() in
  parent.children <- parent.children @ [child];
  child

let add_variable env var =
  ignore(Hashtbl.add env.variables ~key:var ~data:true)

let visited_variable env name =
  ignore(Hashtbl.add env.visited ~key:name ~data:true)

let line_no_from_file file node =
  let ss = node.info.ss in
  let buf = Util.read_file_to_str file in
  let pos = ref 0 in
  let line_no = ref 0 in
  let lines = String.split buf ~on:'\n' in
  while !line_no < (List.length lines) && !pos <= ss do
    let line = List.nth_exn lines !line_no in
    pos := !pos + (String.length line);
    incr line_no
  done;
  !line_no

let rec env_info env =
  Printf.printf "print env .........\n";
  Hashtbl.iter env.variables
               ~f:(fun ~key:v ~data:_ ->
                   let name = name_node_id v in
                   match Hashtbl.find env.visited name with
                   | None ->
                      (Printf.printf "unvisited variable %s(%d) : %s\n"
                                     v.info.file
                                     (line_no_from_file v.info.file v)
                                     name)
                   | _ -> ());
  List.iter env.children ~f:(fun e -> env_info e)

let check_unused asts =
  let rec iter ast env =
    let _iter ast = iter ast env in
    (* let str = Printer.node_to_str ast 0 in *)
    (* Printf.printf "%s\n" str; *)
    match ast.ty with
    | Name(id, _) -> (Printf.printf "lookup: %s\n" id; visited_variable env id)
    | Assign(target, value) -> (
      let _ = match target.ty with
        | Name(s, _) -> (
          Printf.printf "set variable: %s\n" s;
          add_variable env target
        )
        | _ -> () in _iter value
    )
    | Func(info) -> iter info.body (new_child env)
    | Class(_, _, body, _, _) | Module(_, _, body, _) ->  iter body (new_child env)
    | Block(stmts) -> List.iter stmts ~f:_iter
    | Undef(nodes) -> List.iter nodes ~f:_iter
    | BinOp(_, ln, rn) -> List.iter [ln; rn] ~f:_iter
    | UnaryOp(_, operand) -> _iter operand
    | Attribute(target, value) -> _iter target
    | Handler(_, _, handler, _else) -> List.iter [handler; _else] ~f:_iter
    | For(target, it, body) -> List.iter [target; it; body] ~f:_iter
    | If(test, body, _else) -> List.iter [test; body; _else] ~f:_iter
    | While(test, body) -> List.iter [test; body] ~f:_iter
    | Subscript(value, slices) -> List.iter ([value] @ slices) ~f:_iter
    | Try(body, rescue, _else, final) -> List.iter [body; rescue; _else; final] ~f:_iter
    | Call(func, pos, _, block_arg) -> List.iter (([func] @ pos) @ [block_arg]) ~f:_iter
    | Array(elms) -> List.iter elms ~f:_iter
    | Kwd(_, v) | Return(v) | Starred(v) | Yield(v) -> _iter v
    | _ -> () in
  List.iter asts ~f:(fun ast -> iter ast root_env);
  env_info root_env
