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
  let child = make_env() in
  parent.children <- parent.children @ [child];
  child

let add_variable env var =
  Hashtbl.add_exn env.variables ~key:var ~data:true

let visited_variable env name =
  Hashtbl.add_exn env.visited ~key:name ~data:true

let rec env_info env =
  Hashtbl.iter env.variables
               ~f:(fun ~key:v ~data:_ ->
                   let name = name_node_id v in
                   match Hashtbl.find env.visited name with
                   | None -> (Printf.printf "unvisited variable: %s\n" name)
                   | _ -> ());
  List.iter env.children ~f:(fun e -> env_info e)

let check_unused asts =
  let rec iter ast env =
    let str = Printer.node_to_str ast 0 in
    Printf.printf "%s\n" str;
    match ast.ty with
    | Block(stmts) -> (List.iter stmts ~f:(fun ast -> iter ast env))
    | Class(n, _, body, _, _) -> (
      let child = new_child env in
      iter body child
    )
    | Func(info) -> (
      let child = new_child env in
      iter info.body child
    )
    | Name(id, _) -> (
      Printf.printf "lookup: %s\n" id;
      visited_variable env id
    )
    | Assign(target, value) -> (
      match target.ty with
      | Name(s, _) -> (
        Printf.printf "set variable: %s\n" s;
        add_variable env target
      )
      | _ -> ()
    )
    | Call(func, pos, _, _) -> (
      List.iter pos ~f:(fun x -> iter x env)
    )
    | _ -> () in
  List.iter asts ~f:(fun ast -> iter ast root_env);
  env_info root_env
