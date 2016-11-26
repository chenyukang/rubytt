open Core.Std
open Node

type env = {
    mutable visited: (string, bool) Hashtbl.t;
    mutable variables: (Node.node_t, bool) Hashtbl.t;
    mutable children: env list;
  }

let make_env () =
  {
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

let rec env_info env =
  Printf.printf "print env .........\n";
  Hashtbl.iter env.variables
               ~f:(fun ~key:v ~data:_ ->
                   let name = name_node_id v in
                   match Hashtbl.find env.visited name with
                   | None ->
                      (Printf.printf "unvisited variable: %s : %s\n" v.info.file name)
                   | _ -> ());
  List.iter env.children ~f:(fun e -> env_info e)

let check_unused asts =
  let rec iter ast env =
    let str = Printer.node_to_str ast 0 in
    Printf.printf "%s\n" str;
    match ast.ty with
    | Block(stmts) -> (List.iter stmts ~f:(fun ast -> iter ast env))
    | Class(_, _, body, _, _) | Module(_, _, body, _) ->  (
      let child = new_child env in
      iter body child
    )
    | Try(body, rescue, _else, final) -> (
      List.iter [body; rescue; _else; final] ~f:(fun e -> iter e env)
    )
    | Func(info) -> (
      let child = new_child env in
      iter info.body child
    )
    | Undef(nodes) -> List.iter nodes ~f:(fun e -> iter e env)
    | BinOp(_, ln, rn) -> List.iter [ln; rn] ~f:(fun e -> iter e env)
    | UnaryOp(_, operand) -> iter operand env
    | Attribute(target, value) -> iter target env
    | Subscript(value, slices) -> List.iter ([value] @ slices) ~f:(fun e -> iter e env)
    | Handler(_, _, handler, _else) -> List.iter [handler; _else] ~f:(fun e -> iter e env)
    | For(target, it, body) ->
       List.iter [target; it; body] ~f:(fun e -> iter e env)
    | If(test, body, _else) -> List.iter [test; body; _else] ~f:(fun e -> iter e env)
    | While(test, body) -> List.iter [test; body] ~f:(fun e -> iter e env)
    | Name(id, _) -> (
      Printf.printf "lookup: %s\n" id;
      visited_variable env id
    )
    | Assign(target, value) -> (
      let _ = match target.ty with
        | Name(s, _) -> (
          Printf.printf "set variable: %s\n" s;
          add_variable env target
        )
        | _ -> () in
      iter value env
    )
    | Call(func, pos, _, block_arg) ->
       List.iter (([func] @ pos) @ [block_arg]) ~f:(fun e -> iter e env)
    | Kwd(_, v) | Return(v) | Starred(v) | Yield(v) -> iter v env
    | _ -> () in
  List.iter asts ~f:(fun ast -> iter ast root_env);
  env_info root_env
