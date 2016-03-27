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
  Printf.printf "%s\n" ast_str;
  Printf.printf "%s" (gen_ty_str())

let load_dir input_dir output_dir =
  Printf.printf "Dump dir: %s\n%!" input_dir;
  if Sys.file_exists_exn output_dir then
    Sys.command_exn (Printf.sprintf "rm -rf %s;" output_dir);
  Unix.mkdir_p output_dir;
  Sys.command_exn (Printf.sprintf "ruby dump.rb %s %s" input_dir output_dir);
  let jsons = Util.walk_directory_tree output_dir ".*\\.json" in
  List.iter jsons ~f:(fun x ->
      Printf.printf "now: %s\n%!" x;
      let ast = Parser.build_ast_from_json x in
      Analyzer.trans ast;
      let res = gen_ast_str ast in
      let ty_file = Printf.sprintf "%s.ty" (Filename.chop_extension x) in
      Out_channel.write_all ty_file ~data: (res);
      (* Global.print_size(); *)
    )

let dump_dot() =
  let dot_res = Dot.node_to_dot_str Type.global_table in
  Out_channel.write_all "dep.dot" ~data:dot_res;
  Sys.command_exn "dot dep.dot -Tpng -o dep.png; open dep.png"

let dump_html input output_dir =
  let root_dir = match Sys.is_directory_exn input with
    | true -> input
    | _ -> Sys.getcwd() in
  new_linker root_dir output_dir;
  Printf.printf "here: %s %s\n" root_dir output_dir;
  find_links !Global.bindings

let () =
  let arr = Array.filter ~f:(fun x -> not(String.is_prefix x "-")) Sys.argv in
  let len = Array.length arr in
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
    if Array.mem Sys.argv "-dot" then
      dump_dot();
    if Array.mem Sys.argv "-html" then
      dump_html filename arr.(2)



