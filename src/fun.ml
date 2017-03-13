open Core.Std
open Node

type func_stat =
  {
    ast : Node.node_t;
    cnt : int;
  }

let stats = ref []
            
let add_fun_info ast cnt =
  stats := !stats @ [{ast = ast; cnt = cnt}]
  
let traverse asts =
  let rec iter ast cnt =
    let _iter ast = iter ast 0 in
    let _iter_list asts =
      List.fold asts ~init:1 ~f:(fun acc x -> acc + _iter x) in
    match ast.ty with
    | Name(id, _) -> cnt + 1
    | Assign(target, value) -> (
      let target_cnt = match target.ty with
        | Name(_, _) -> 1
        | Array(ls) -> _iter_list ls                     
        | _ -> (iter target cnt) in
      target_cnt + (iter value cnt)
    )
    | Func(info) -> (
      let args_cnt = _iter_list info.args in
      let body_cnt = iter info.body 0 in
      let all = (args_cnt + body_cnt) in
      if is_lambda ast = false then 
        add_fun_info ast all;
      all
    )
    | Class(name, _, body, _, _)
      | Module(name, _, body, _) -> (
      iter body cnt
    )
    | Block(stmts) -> _iter_list stmts
    | Undef(nodes) -> _iter_list nodes
    | BinOp(_, ln, rn) -> _iter_list [ln;rn]
    | UnaryOp(_, operand) -> 1 + _iter operand
    | Attribute(target, _) -> 1 + _iter target
    | StrEmbed(value) -> 1 + _iter value
    | Handler(_, _, handler, _else) -> _iter_list [handler; _else]
    | For(target, it, body) -> _iter_list [target; it; body]
    | If(test, body, _else) -> _iter_list [test; body; _else]
    | While(test, body) -> _iter_list [test; body]
    | Subscript(value, slices) -> _iter_list ([value] @ slices)
    | Try(body, rescue, _else, final) -> _iter_list [body; rescue; _else; final]
    | Call(func, pos, star, block_arg) -> _iter_list (([star] @ pos) @ [block_arg])
    | Array(elms) -> _iter_list elms
    | Dict(ks, vs) -> _iter_list (ks @ vs)
    | Kwd(_, v) | Return(v) | Starred(v) | Yield(v) -> _iter v
    | _ -> 0 in
  List.iter asts ~f:(fun ast -> ignore(iter ast 0));
  stats := List.sort !stats ~cmp:(fun a b -> b.cnt - a.cnt);
  let res = ref "" in
  List.iteri !stats ~f:(fun i info ->
               if i <= 20 then (
                 let ast = info.ast in
                 let cnt = info.cnt in
                 let path, line, name = Node.node_to_info ast in
                 let line = Printf.sprintf "%-80s %3d %-30s %d\n"
                                           path line name cnt in
                 res := !res ^ line));
  Printf.printf "%s\n" !res
