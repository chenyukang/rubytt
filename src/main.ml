open Core.Std
open Sys

let load_file file =
  let json = Parser.run_dump_ruby file in
  let ast = Parser.build_ast_from_file json in
  let _ = Analyzer.trans ast in
  let ast_str = Printer.node_to_str ast 0 in
  Printf.printf "%s\n" ast_str;
  Printf.printf "\n%s\n\n" (Printer.table_to_str Type.global_table 0)

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: main filename\n"
  else
    let filename = Sys.argv.(1) in
    load_file filename

