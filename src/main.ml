open Core.Std
open Sys
open Parser
open Printer
open Node
open Util
open Type
open Analyzer

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: main filename\n"
  else
    let filename = Sys.argv.(1) in
    Analyzer.load_file filename
    (* let json = run_dump_ruby filename in *)
    (* let ast = build_ast_from_file json in *)
    (* let ast_str = node_to_str ast 0 in *)
    (* Printf.printf "%s\n" ast_str *)

