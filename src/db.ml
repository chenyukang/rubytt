open Core.Std
open Node

let tables = Hashtbl.Poly.create();;

let rec iter ast =
  match ast.ty with
  | Block(stmts) ->
    List.iter stmts ~f:(fun s -> iter s)
  | Call(name, _ , _, block_arg) -> (
    match name.ty with
    | Name(s, _) -> (
        if compare_string s "create_table" = 0 then (
          build_table ast
        )
      )
    | _ -> iter block_arg
    )
  | Func(info) -> iter info.body
  | _ -> ()
and
  build_table ast =
  match ast.ty with
  |Call(name, args, _, block_arg) -> (
      let name = List.nth_exn args 0 in
      Printf.printf "table: %s\n" (Node.name_node_id name);
      let columns = table_columns block_arg in
      Printf.printf "columns: \n";
      List.iter columns ~f:(fun c -> Printf.printf " %s " c);
      Hashtbl.add_exn tables name columns;
    )
  | _ -> failwith "invalid node in build_table"
and
  table_columns block_arg =
  match block_arg.ty with
  | Func(info) -> (
    match info.body.ty with
    | Block(stmts) ->
      List.map stmts ~f:(fun c ->
          match c.ty with
          | Call(_, args, _, _) ->
            let name = List.nth_exn args 0 in
            Node.name_node_id name
          | _ -> ""
        )
    | _ -> failwith "invalid type in table_columns"
    )
  | _ -> failwith "invalid type in table_columns"



let node_to_dot_str ast =
  let res = Printer.node_to_str ast 0 in
  Printf.printf "now: %s\n" res;
  iter ast;
  ""

let dump_db ast =
  let dot_res = node_to_dot_str ast in
  Out_channel.write_all "db.dot" ~data: dot_res
  (* Sys.command_exn "dot db.dot -Tpng -o db.png; open db.png" *)

