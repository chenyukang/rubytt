open Core.Std
open Sys
open Parser
open Printer
open Node
open Util
open Type
open Typestack

let () =
  let a = TypeStack.empty in
  let a = TypeStack.push a Nil (Int 2) in (
    let b = TypeStack.pop a in
    let b = TypeStack.push b (Int 2) Void in 
    let e = TypeStack.size b in
    Printf.printf "size: %d\n" e;
    if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: main filename\n"
  else
    let filename = Sys.argv.(1) in
    let json = run_dump_ruby filename in
    let ast = build_ast_from_file json in
    let ast_str = node_to_str ast 0 in
    Printf.printf "%s\n" ast_str
  )
