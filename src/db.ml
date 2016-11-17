open Core.Std
open Node

let tables = Hashtbl.Poly.create();;
let belongs_table = Hashtbl.Poly.create();;
let has_one_table = Hashtbl.Poly.create();;

let analysis_model_ast ast =
  let rec iter ast =
    match ast.ty with
    | Block(stmts) ->
      List.iter stmts ~f:(fun s -> iter s)
    | Class(n, _, body, _, static) -> (
      let res = Printer.node_to_str ast 0 in
      let name = Node.name_node_id n in (
        Printf.printf "now: %s\n" res;
        Printf.printf "class: %s\n" name
      );
      process_body name body
      )
    |_ -> ()
  and
    process_body model_name node =
    match node.ty with
    | Block(stmts) ->
      (List.iter stmts ~f:(fun s ->
           (match s.ty with
            | (Call(name, pos_args, _, _)) -> (
                let key_name = Node.name_node_id name in
                match key_name with
                | "belongs_to" -> (
                    let arg_name = first_arg_name pos_args key_name in
                    Printf.printf "%s belongs_to %s\n" model_name arg_name;
                    add_relation_ship belongs_table model_name arg_name
                  )
                | "has_one" -> (
                    let arg_name = first_arg_name pos_args key_name in
                    Printf.printf "%s has_one %s\n" model_name arg_name;
                    add_relation_ship has_one_table model_name arg_name
                  )
                | _ -> ()
              )
            | _ -> ()
           )
         ))
    | _ -> ()
  and
    first_arg_name pos_args key_name =
    let arg_node = List.nth pos_args 0 in
    match arg_node with
    | Some(arg) -> (Node.symbol_to_str arg)
    | _ -> failwith (Printf.sprintf "belongs_to dont have arg: %s\n" key_name)
  and
    add_relation_ship hash a b =
    match Hashtbl.find hash a with
    | Some(v) -> Hashtbl.set hash a (b :: v)
    | _ -> Hashtbl.add_exn hash a ([b])
  in
  iter ast

let analysis_db_ast ast =
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
    | Call(name, args, _, block_arg) -> (
        let name = List.nth_exn args 0 in
        let columns = table_columns block_arg in
        (* Printf.printf "columns: \n"; *)
        (* List.iter columns ~f:(fun c -> Printf.printf " %s " c); *)
        Hashtbl.add_exn tables (string_of_str name) columns;
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
              | Call(attr, args, _, _) ->
                let name = List.nth_exn args 0 in
                [|(string_of_str name); (string_of_attr attr)|]
              | _ -> [||]
            )
        | _ -> failwith "invalid type in table_columns"
      )
    | _ -> failwith "invalid type in table_columns"
  and
    string_of_str str =
    match str.ty with
    | String(s) -> s
    | _ -> failwith "invalid type in string_of_str"
  and
    string_of_attr attr =
    match attr.ty with
    | Attribute(t, v) ->
      (name_node_id v)
    | _ -> failwith "invalid type in string_of_attr"
  in
  iter ast

let wrapper content =
  "digraph G {
    rankdir = \"TB\";
    ranksep = \"0.5\";
    nodesep = \"0.4\";
    pad = \"0.4,0.4\";
    margin = \"0,0\";
    concentrate = \"true\";
    labelloc = \"t\";
    fontsize = \"13\";
    fontname = \"Arial BoldMT\";
    node[ shape  =  \"Mrecord\" , fontsize  =  \"10\" , \
    fontname  =  \"ArialMT\" , margin  =  \"0.07,0.05\" , penwidth  =  \"1.0\"];
    edge[ fontname  =  \"ArialMT\" , fontsize  =  \"7\" , dir  =  \"both\" , \
    arrowsize  =  \"0.9\" , penwidth  =  \"1.0\" , labelangle  =  \"32\" , \
    labeldistance  =  \"1.8\"];
    rankdir = \"TB\";
  " ^ content ^ "\n}\n"

let gen_id model =
  Printf.sprintf "M_%s" model

let db_to_dot_str ast =
  analysis_db_ast ast;
  let content = ref "" in
  Hashtbl.iter tables ~f:(fun ~key:k ~data:v ->
      content := !content ^ (
          "\n    " ^ (gen_id k) ^ "[label=\"{{" ^ k ^ "}" ^
          (List.fold v ~init:"" ~f:(fun acc x ->
               let name_with_type = x.(0) ^ " : " ^ x.(1) in
               acc ^ "|{" ^ name_with_type ^ "}")) ^
          "}\",  color=blue, fontcolor=blue]\n"
        )
    );
  wrapper !content

let dump_db ast =
  let dot_res = db_to_dot_str ast in
  Out_channel.write_all "db.dot" ~data: dot_res
  (* Sys.command_exn "dot db.dot -Tpng -o db.png; open db.png" *)

