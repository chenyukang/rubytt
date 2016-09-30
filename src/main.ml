open Core.Std
open Sys
open Linker

let run file =
  let json = Parser.run_dump_ruby file in
  let ast = Parser.build_ast_from_json json in
  Analyzer.trans ast;
  ast

let gen_ast_str ast =
  Printer.node_to_str ast 0

let gen_ty_str() =
  let ty_str = Printer.table_to_str Type.global_table 0 in
  Printf.sprintf "%s\n\n" ty_str

let load_file file =
  let ast = run file in
  let ast_str = gen_ast_str ast in
  Global.set_load_file file;
  Printf.eprintf "%s\n" ast_str;
  Printf.eprintf "%s" (gen_ty_str())

let load_dir input_dir output_dir =
  Printf.printf "Dump dir: %s\n%!" input_dir;
  if Sys.file_exists_exn output_dir then
    Sys.command_exn (Printf.sprintf "rm -rf %s;" output_dir);
  Unix.mkdir_p output_dir;
  Sys.command_exn (Printf.sprintf "ruby dump.rb %s %s" input_dir output_dir);
  let rb_files = Util.walk_directory_tree input_dir ".*\\.rb" in
  List.iter rb_files ~f:(fun x -> Global.set_load_file x);
  let jsons = Util.walk_directory_tree output_dir ".*\\.json" in
  List.iter jsons ~f:(fun x ->
      let ast = Parser.build_ast_from_json x in
      Analyzer.trans ast;
      let res = gen_ast_str ast in
      let ty_file = Printf.sprintf "%s.ty" (Filename.chop_extension x) in
      Out_channel.write_all ty_file ~data: res;
    );
  Global.print_size()

let load_db_file input_dir =
  if (Sys.is_directory_exn input_dir) = false then
    Printf.eprintf "Please set the Rails project root directory\n"
  else (
    let db_schema = input_dir ^ "/db/schema.rb" in
    if (Sys.is_file_exn db_schema) then (
      Printf.printf "%s" db_schema;
      let ast = run db_schema in
      Db.dump_db ast
    ) else
      Printf.eprintf "%s does not exits\n" db_schema
  )

let () =
  let arr = Array.filter ~f:(fun x -> not(String.is_prefix x "-")) Sys.argv in
  let len = Array.length arr in
  if len <> 2 && len <> 3 then (
    let info = "Usage: rubytt <options> filename\n" ^
               "Options are:\n" ^
               " -dot  generate a dot for visualize project\n" ^
               " -db   generate model from db/schema.rb\n" ^
               " -html generate HTML files after type analysis\n" in
    Printf.eprintf "%s\n" info
  )
  else
    let filename = arr.(1) in
    if not (Sys.file_exists_exn filename) then
      Printf.eprintf "Filename: %s does not exists\n" filename
    else (
      if Array.mem Sys.argv "-db" then (
        load_db_file filename
      ) else (
        if Sys.is_directory_exn filename then
          if len <> 3 then
            Printf.eprintf "Please set output directory name\n"
          else
            load_dir filename arr.(2)
        else
          load_file filename
      );
      if Array.mem Sys.argv "-dot" then
        Dot.dump_dot();

      if Array.mem Sys.argv "-html" then
        Html.dump_html filename arr.(2)
    )
