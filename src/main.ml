open Core.Std
open Sys

let run file =
  let json = Parser.run_dump_ruby file in
  let ast = Parser.build_ast_from_file json in
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
  Printf.printf "%s\n" ast_str;
  Printf.printf "%s" (gen_ty_str())

let load_dir input_dir output_dir =
  if Sys.file_exists_exn output_dir then
    Sys.command_exn (Printf.sprintf "rm -rf %s;" output_dir);
  Unix.mkdir_p output_dir;
  let results = Util.walk_directory_tree input_dir ".*\\.rb" in
  let real_input_dir = Filename.realpath input_dir in
  let real_output_dir = Filename.realpath output_dir in
  Printf.eprintf "real_in: %s\n%!" real_input_dir;
  Printf.eprintf "real_ot: %s\n%!" real_output_dir;
  List.iter results ~f:(fun x ->
      let real_path = Filename.realpath x in
      let new_path = Str.replace_first (Str.regexp_string real_input_dir) output_dir real_path in
      let ast = run x in
      let res = gen_ast_str ast in 
      let dir = Filename.dirname new_path in
      Printf.eprintf "%s\n%!" real_path;
      (* Global.print_size(); *)
      if Sys.file_exists_exn dir = false then
        Unix.mkdir_p dir;
      Out_channel.write_all new_path ~data: (res);
    )

let dump_dot() =
  let dot_res = Dot.node_to_dot_str Type.global_table in
  Out_channel.write_all "dep.dot" ~data:dot_res;
  Sys.command_exn "dot dep.dot -Tpng -o dep.png; open dep.png"

let () =
  let arr = Array.filter ~f:(fun x -> not(String.is_prefix x "-")) Sys.argv in
  let len = Array.length arr in
  Printf.printf "len: %d\n" len;
  if len <> 2 && len <> 3 then
    Printf.eprintf "Usage: main filename\n"
  else
    let filename = arr.(1) in
    if not (Sys.file_exists_exn filename) then
      Printf.eprintf "Filename: %s does not exists\n" filename
    else (
      if Sys.is_directory_exn filename then
        if len <> 3 then
          Printf.eprintf "Please set output directory name\n"
        else
          load_dir filename arr.(2)
      else
        load_file filename
    );
    if Array.mem Sys.argv "-dot" then (
      Printf.printf "hereee\n";
      dump_dot())

