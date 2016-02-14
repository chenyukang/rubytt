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

