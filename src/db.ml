open Core.Std
open Node

let tables = Hashtbl.Poly.create();;
let belongs_table = Hashtbl.Poly.create();;
let has_one_table = Hashtbl.Poly.create();;
let has_many_table = Hashtbl.Poly.create();;
let model_db_name = Hashtbl.Poly.create();;

let analysis_model_ast ast =
  let rec iter ast =
    match ast.ty with
    | Block(stmts) ->
      List.iter stmts ~f:(fun s -> iter s)
    | Class(n, _, body, _, _) -> (
      let name = Node.name_node_id n in
      process_body name body
      )
    |_ -> ()
  and
    process_body model_name node =
    match node.ty with
    | Block(stmts) ->
      (List.iter stmts ~f:(fun s ->
           let _s = Printer.node_to_str s 0 in
           Printf.printf "now: %s\n" _s;
           (match s.ty with
            | (Call(name, pos_args, _, _)) -> (
                let key_name = Node.name_node_id name in
                let db_name = Util.class_to_table_name model_name in
                match key_name with
                | "belongs_to" | "has_one" | "has_many" -> (
                    let arg_name = optional_class_name pos_args key_name in
                    let table = match_table key_name in
                    Printf.printf "%s %s %s\n" db_name key_name arg_name;
                    add_relation table db_name arg_name
                  )
                | _ -> ()
              )
            | (Assign(target, value)) -> (
                match target.ty with
                | Attribute(v, at) -> (
                    match (attr_id v), (attr_id at), value.ty with
                    | "self", "table_name", String(n) -> Hashtbl.add_exn model_db_name model_name n
                    | _ -> ()
                  )
                | _ -> ()
              )
            | _ -> ()
           )
         ))
    | _ -> ()
  and
    match_table key =
    match key with
    | "belongs_to" -> belongs_table
    | "has_one" -> has_one_table
    | "has_many" -> has_many_table
    | _ -> failwith "invalid table"
  and
    first_arg_name pos_args key =
    let arg_node = List.nth pos_args 0 in
    match arg_node with
    | Some(arg) -> (
        let name = Node.symbol_to_str arg in
        match key with
        | "has_many" -> name
        | _ -> Util.class_to_table_name name
      )
    | _ -> failwith (Printf.sprintf "%s dont have arg" key)
  and
    optional_class_name pos_args cur_key =
    let arg = List.nth pos_args 1 in
    match class_name_arg arg with
    | Some(s) -> s
    | _ -> first_arg_name pos_args cur_key
  and
    class_name_arg arg_node =
    match arg_node with
    | Some(arg) -> (
        (* let str = Printer.node_to_str arg 0 in *)
        (* Printf.printf "now: %s\n" str; *)
        match arg.ty with
        | Dict(keys, vals) -> (
            let res = ref "" in
            let i = ref 0 in
            while !res = "" && !i < (List.length keys) do
              (match (List.nth keys !i), (List.nth vals !i) with
              | Some(k), Some(v) -> (
                  match k.ty, v.ty with
                  | Name("class_name", _), String(s)  -> (res := Util.class_to_table_name s)
                  | _, _ -> ()
                )
              | _ -> ());
              incr i
            done;
            if !res = "" then None else Some(!res)
          )
        | _ -> None
      )
    | _ -> None
  and
    add_relation hash a b =
    match Hashtbl.find hash a with
    | Some(v) -> Hashtbl.set hash ~key:a ~data:(b :: v)
    | _ -> Hashtbl.add_exn hash ~key:a ~data:[b]
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
    | Call(_, args, _, block_arg) -> (
        let name = List.nth_exn args 0 in
        let columns = table_columns block_arg in
        Hashtbl.add_exn tables ~key:(string_of_str name) ~data:columns;
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
    | Attribute(_, v) ->
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

let db_to_dot_str() =
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
  Hashtbl.iter has_one_table ~f:(fun ~key:k ~data:vals ->
      List.iter vals ~f:(fun v ->
          (* (Printf.printf "key table: %s -> %s\n" k v); *)
          content := !content ^ (
              Printf.sprintf "M_%s -> M_%s [color=\"red\"]\n" k v
            )
        );
    );
  Hashtbl.iter belongs_table ~f:(fun ~key:k ~data:vals ->
      List.iter vals ~f:(fun v ->
          (* (Printf.printf "key table: %s -> %s\n" k v); *)
          match Hashtbl.find has_one_table v with
          | Some(k) -> ()
          | _ -> (
              content := !content ^ (
                  Printf.sprintf "M_%s -> M_%s [color=\"red\"]\n" k v
                )
            )
        );
    );
  Hashtbl.iter has_many_table ~f:(fun ~key:k ~data:vals ->
      List.iter vals ~f:(fun v ->
          (* (Printf.printf "key table: %s -> %s\n" k v); *)
          content := !content ^ (
              Printf.sprintf "M_%s -> M_%s [color=\"orange\"]\n" k v
            )
        );
    );
  wrapper !content

let dump_db output =
  let dot_res = db_to_dot_str() in
  Out_channel.write_all "db.dot" ~data: dot_res;
  Sys.command_exn (Printf.sprintf "dot db.dot -Tpng -o %s" output);
  Sys.command_exn ("open " ^ output)
