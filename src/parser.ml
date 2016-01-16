open Core.Std
open Yojson
open Yojson.Basic.Util
open Yojson.Basic
open UnixLabels
open Node

let rec convert json =
  match json with
  | `Null -> nil_node
  | _ -> nil_node

let build_ast_from_file file =
  let json = Yojson.Basic.from_file file in
  convert json

