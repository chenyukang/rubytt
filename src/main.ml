open Core.Std
open Sys

let parse_to_ast ?need_trans:(need_trans=true) file =
  let json = Parser.run_dump_ruby file in
  let ast = Parser.build_ast_from_json json in
  if need_trans then Analyzer.trans ast;
  ast

let gen_ast_str ast =
  Printer.node_to_str ast 0

let load_dir ?need_trans:(need_trans=true) input_dir output_dir =
  Printf.printf "Dump dir: %s\n%!" input_dir;
  if Sys.file_exists_exn output_dir then
    Sys.command_exn (Printf.sprintf "rm -rf %s;" output_dir);
  Unix.mkdir_p output_dir;
  Sys.command_exn (Printf.sprintf "ruby dump.rb %s %s" input_dir output_dir);
  let rb_files = Util.walk_directory_tree input_dir ".*\\.rb" in
  let rb_files = List.filter
      ~f:(fun rb -> not((Util.contains rb "/spec/") ||
                        (Util.contains rb "/migrate/"))
         ) rb_files in
  (* List.iter rb_files ~f:(fun x -> Printf.printf "now processing: %s\n" x); *)
  List.iter rb_files ~f:(fun x -> Global.set_load_file x);
  let jsons = Util.walk_directory_tree output_dir ".*\\.json" in
  let asts = List.map jsons ~f:(fun x ->
      let ast = Parser.build_ast_from_json x in
      if need_trans then (
        Analyzer.trans ast;
        let res = gen_ast_str ast in
        let ty_file = Printf.sprintf "%s.ty" (Filename.chop_extension x) in
        Out_channel.write_all ty_file ~data: res;
      );
      ast
    ) in
  Global.print_size();
  asts

let load_db ?dump_db:(dump_db=false) input_dir output =
  if (Sys.is_directory_exn input_dir) = false then
    Printf.eprintf "Please set the Rails project root directory\n"
  else (
    if dump_db then (
      let db_schema = input_dir ^ "/db/schema.rb" in
      if not (Sys.is_file_exn db_schema) then
        failwith (Printf.sprintf "File %s does not exits" db_schema);
      let db_ast = parse_to_ast db_schema in
      Db.analysis_db_ast db_ast
    );
    let model_dir = input_dir ^ "/app/models" in
    if not (Sys.is_directory_exn model_dir) then
      failwith (Printf.sprintf "Dir: %s does not exits" model_dir);
    let asts = load_dir ~need_trans:false model_dir "/tmp/rubytt/model/" in
    List.iter asts ~f:(fun ast -> Db.analysis_model_ast ast);
    Db.dump_db output
  )


let command =
  Command.basic
    ~summary: "rubytt an Ruby analyser"
    Command.Spec.(
      empty
      +> flag "-s" (optional string) ~doc:"the source code directory"
      +> flag "-t" (optional string) ~doc:"the analysis type, shoud in [class, db, model, type]"
      +> flag "-o" (optional string) ~doc:"the output directory or file"
    )
    (fun source_code analy_type output () ->
       match source_code with
       | Some(source) -> (
           Printf.printf "source: %s\n" source;
           if not(Sys.is_directory_exn source) then
             failwith (Printf.sprintf "%s is not an directory\n" source);
           let output = match output with
             |Some(s) -> s
             |_ -> failwith "Please set output directory or file\n" in
           match analy_type with
           | Some("class") -> (
               ignore(load_dir source "/tmp/rubytt/");
               Class.dump_class_dot()
             )
           | Some("db") -> load_db ~dump_db:true source output
           | Some("model") -> load_db ~dump_db:false source output
           | Some("type") -> (
               ignore(load_dir source output);
               Html.dump_html source output
             )
           | _ -> failwith "Invalid option"
         )
       | _ -> (
           Printf.printf "Please input source code directory\n"
         )
    )

let () = Command.run command

