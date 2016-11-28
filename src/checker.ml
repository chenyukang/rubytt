open Core.Std
open Node

type env = {
  env_ty : string;
  mutable visited: (string, bool) Hashtbl.t;
  mutable variables: (Node.node_t, bool) Hashtbl.t;
  mutable children: env list;
}

let make_env ?ty:(ty="normal") ()=
  {
    env_ty = ty;
    visited = Hashtbl.Poly.create();
    variables = Hashtbl.Poly.create();
    children = [];
  }

let root_env = ref (make_env());;

let new_child ?ty:(ty="normal") parent =
  let child = (make_env ~ty:ty ()) in
  parent.children <- parent.children @ [child];
  child

let clear () =
  root_env := make_env()

let add_variable env var =
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

let rec env_info env =
  let cur_dir = Sys.getcwd () in
  let res = ref "" in
  let rec env_visited is_global name env =
    ((Hashtbl.find env.visited name) <> None) ||
    (List.find env.children
       ~f:(fun e ->
           (* global variable will check all scope *)
           (* let is_glo_str = if is_global then "true" else "false" in *)
           (* Printf.printf "check children in: %s %s\n" name is_glo_str; *)
           if is_global then env_visited is_global name e
           else (e.env_ty <> "func") && (env_visited is_global name e)
         )
     <> None) in
  Hashtbl.iter env.variables
    ~f:(fun ~key:v ~data:_ ->
        let name = name_node_id v in
        let is_global = name_is_global v in
        if (env_visited is_global name env) = false then
          res := !res ^
                 (Printf.sprintf "unvisited variable %s(%d) : %s\n"
                    (Stringext.replace_all v.info.file ~pattern:cur_dir ~with_:".")
                    (line_no_from_file v.info.file v)
                    name));
  List.iter env.children ~f:(fun e -> res := !res ^ (env_info e));
  !res

let check_result () =
  env_info !root_env

let print_env_info env =
  Printf.printf "%s" (env_info env)

let check_unused asts =
  let rec iter ast env =
    let _iter ast = iter ast env in
    (* let str = Printer.node_to_str ast 0 in *)
    (* Printf.printf "%s\n" str; *)
    match ast.ty with
    | Name(id, _) -> (
      (* Printf.printf "lookup: %s\n" id; *)
      visited_variable env id
    )
    | Assign(target, value) -> (
      let _ = match target.ty with
        | Name(s, t) -> (
          (* Printf.printf "set variable: %s\n" s; *)
          match t with
          | Local | Global -> add_variable env target
          | _ -> ()
        )
        | _ -> () in _iter value
    )
    | Func(info) -> (
        let child_ty = match is_lambda ast with
          | true -> "normal"
          | _ -> "func" in
        iter info.body (new_child ~ty:child_ty env)
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
    | Call(func, pos, _, block_arg) -> List.iter (([func] @ pos) @ [block_arg]) ~f:_iter
    | Array(elms) -> List.iter elms ~f:_iter
    | Dict(ks, vs) -> List.iter (ks @ vs) ~f:_iter
    | Kwd(_, v) | Return(v) | Starred(v) | Yield(v) -> _iter v
    | _ -> () in
  List.iter asts ~f:(fun ast -> iter ast !root_env);
  print_env_info !root_env
