open Core.Std
open Sys

let run file =
  let json = Parser.run_dump_ruby file in
  let ast = Parser.build_ast_from_file json in
  let _ = Analyzer.trans ast in
  let ast_str = Printer.node_to_str ast 0 in
  let ty_str = Printer.table_to_str Type.global_table 0 in
  Printf.sprintf "%s\n\n %s\n\n" ast_str ty_str

let load_file file =
  let run_res = run file in
  Printf.printf "%s" run_res

let load_dir input_dir output_dir =
  (* let files = Array.to_list (Sys.readdir input_dir) in *)
  if Sys.file_exists_exn output_dir then
    Unix.rmdir output_dir;
  Unix.mkdir_p output_dir;
  let results = Util.walk_directory_tree input_dir ".*\\.rb" in
  let real_input_dir = Filename.realpath input_dir in
  let real_output_dir = Filename.realpath output_dir in
  Printf.printf "real_in: %s\n" real_input_dir;
  Printf.printf "real_ot: %s\n" real_output_dir;
  List.iter results ~f:(fun x ->
      let real_path = Filename.realpath x in
      let new_path = Str.replace_first (Str.regexp_string real_input_dir) output_dir real_path in
      Printf.printf "real_path: %s new_path: %s\n" real_path new_path;
      Printf.printf "parent: %s name: %s\n" (Filename.realpath x) x;
      let res = run x in
      let dir = Filename.dirname new_path in
      if Sys.file_exists_exn dir = false then
        Unix.mkdir_p dir;
      Out_channel.write_all new_path ~data: (res);
    )

let () =
  let len = Array.length Sys.argv in
  Printf.printf "len: %d\n" len;
  if len <> 2 && len <> 3 then
    Printf.eprintf "Usage: main filename\n"
  else
    let filename = Sys.argv.(1) in
    if Sys.file_exists_exn filename = false then
      Printf.eprintf "Filename: %s does not exists\n" filename
    else (
      if Sys.is_directory_exn filename then
        if len <> 3 then
          Printf.eprintf "Please input output directory name\n"
        else
          load_dir filename Sys.argv.(2)
      else
        load_file filename
    )

