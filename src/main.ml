open Core.Std
open Sys
open Parser
open Printer
open Node
open Util
open Type
open Analyzer

let load_file file =
  let json = run_dump_ruby file in
  let ast = build_ast_from_file json in
  ignore(Trans.transform_expr ast Type.global_table);
  let ast_str = Printer.node_to_str ast 0 in
  Printf.printf "%s\n" ast_str

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: main filename\n"
  else
    let filename = Sys.argv.(1) in
    load_file filename

