open Core.Std
open Yojson
open Yojson.Basic.Util
open Yojson.Basic
open UnixLabels
open Node

let to_s s =
  let r = s |> to_string in
  let x = String.drop_prefix r 1 in
  String.drop_suffix x 1

let rec convert json =
  let ty = json |> member "type" |> to_s in
  let ss = json |> member "start" |> to_int in
  let ee = json |> member "end" |> to_int in
  match ty with
  | "program" ->
    Printf.printf "ty: %s ss: %d ee: %d\n" ty ss ee;
    let body = json |> member "body" in
    convert body
  | "block" ->
    Printf.printf "ty: %s\n" ty;
  | _ -> Printf.printf "here\n"

let build_ast_from_file file =
  let json = Yojson.Basic.from_file file in
  convert json


let run() =
  build_ast_from_file "./ruby.json"
