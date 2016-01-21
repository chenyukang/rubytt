open Core.Std
open Sys
open Parser
open Printer
open Node
open Util


let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: main filename\n"
  else
    let filename = Sys.argv.(1) in
    let ast = parse_file filename in
    let ast_str = node_to_str ast 0 in
    Printf.printf "%s\n" ast_str
