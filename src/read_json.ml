(* corebuild -pkg yojson dji.native *)
open Core.Std
open Yojson
open Printf
open Yojson.Basic.Util
open UnixLabels

open Yojson.Basic

let read_json() =
  Yojson.Basic.from_file "./ruby.json"

let rec walker j =
  match j with
  | `Assoc(a) ->
    List.iter a ~f:(fun x ->
        match x with
        | (s, v) ->  (
            Printf.printf "%s\n" s;
            walker v)
        | _ -> ());
  | `List(a) -> List.iter a ~f:(fun x -> walker x)
  | `Int(v) -> Printf.printf "int: %d\n" v
  | `String(str) -> Printf.printf "str: %s\n" str
  | `Bool v -> string_of_bool v |> Printf.printf "bool: %s\n"
  | `Null -> Printf.printf "null\n"
  | _ -> Printf.printf "else\n";;

let res = read_json() in
walker res
