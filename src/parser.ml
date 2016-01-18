open Core.Std
open Yojson
open Yojson.Basic.Util
open Yojson.Basic
open UnixLabels
open Node

let to_s s =
  let r = to_string s in
  let x = String.drop_prefix r 1 in
  String.drop_suffix x 1

let rec convert json =
  let ty = json |> member "type" |> to_s in
  let ss = json |> member "start" |> to_int in
  let ee = json |> member "end" |> to_int in
  (* Printf.printf "now type: %s" ty; *)
  match ty with
  | "program" ->
    (* Printf.printf "ty: %s ss: %d ee: %d\n" ty ss ee; *)
    let body = json |> member "body" in
    convert body
  | "block" ->
    (* Printf.printf "ty: %s\n" ty; *)
    let stmts = convert_list (json |> member "stmts") in
    make_block_node stmts "file" ss ee
  | "int" ->
    let v = json |> member "value" |> to_s in
    Printf.printf "here int value: %s\n" v;
    make_int_node v "file" ss ee  (* FIXME *)
  | "float" ->
    let v = json |> member "value" |> to_s in
    Printf.printf "here float value: %s\n" v;
    make_float_node v "file" ss ee (* FIXME *)
  | "symbol" ->
    let sym = json |> member "id" |> to_s in
    make_symbol_node sym "file" ss ee
  | "string" ->
    let str = json |> member "id" |> to_s in
    make_string_node str "file" ss ee
  | "name" ->
    let id = json |> member "id" |> to_s in
    make_name_node id "file" ss ee (* FIXME *)
  | "unary" ->
    let op = convert_op (json |> member "op") in
    let operand = convert (json |> member "operand") in
    make_unary_node op operand "file" ss ee
  | "yield" ->
    let value = convert (json |> member "value") in
    make_yield_node value "file" ss ee
  | "return" ->
    let value = convert (json |> member "value") in
    make_return_node value "file" ss ee
  | "while" ->
    let test = convert (json |> member "test") in
    let body = convert (json |> member "body") in
    make_while_node test body "file" ss ee
  | "assign" ->
    let _var = convert (json |> member "target") in
    let _val = convert (json |> member "value") in
    make_assign_node _var _val "file" ss ee
  | "begin" ->
    let body = convert (json |> member "body") in
    let _rescue = convert (json |> member "rescue") in
    let orelse = convert (json |> member "else") in
    let final = convert (json |> member "ensure") in
    make_try_node body _rescue orelse final "file" ss ee
  | "binary" -> (
    let l = convert (json |> member "left") in
    let r = convert (json |> member "right") in
    let op = convert_op (json |> member "op") in
    match op with
    | LtE ->
      let lt = make_bin_node Node.Lt l r "file" ss ee in
      let eq = make_bin_node Node.Eq l r "file" ss ee in
      make_bin_node Or lt eq "file" ss ee
    | GtE ->
      let gt = make_bin_node Node.Gt l r "file" ss ee in
      let eq = make_bin_node Node.Eq l r "file" ss ee in
      make_bin_node Node.Or gt eq "file" ss ee
    | NotEqual ->
      let eq = make_bin_node Node.Equal l r "file" ss ee in
      make_unary_node Node.Not eq "file" ss ee
    | NotMatch ->
      let eq = make_bin_node Match l r "file" ss ee in
      make_unary_node Node.Not eq "file" ss ee
    | _ -> make_bin_node op l r "file" ss ee
    )
  | _ -> Printf.printf "here\n"; nil_node
and
  convert_list stmts =
  match stmts with
  | `List(v) ->
    List.map v ~f:(fun e -> convert e)
  | _ -> Printf.printf "type error in convert_list\n"; []
and
  convert_op op =
  let o = op |> member "name" |> to_s in
  Printf.printf "now: %s\n" o;
  match o with
  | "+" | "+@" -> Node.Add
  | "-" | "-@" -> Node.Sub
  | "<=>" -> Node.Cmp
  | "*" -> Node.Mul
  | "/" -> Node.Div
  | "**" -> Node.Pow
  | "=~" -> Node.Match
  | "!~" -> Node.NotMatch
  | "==" | "===" -> Node.Equal
  | "<" -> Node.Lt
  | ">" -> Node.Gt
  | "<=" -> Node.LtE
  | ">=" -> Node.GtE
  | "&" -> Node.BitAnd
  | "|" -> Node.BitOr
  | "^" -> Node.BitXor
  | "in" -> Node.In
  | "<<" -> Node.LShift
  | ">>" -> Node.RShift
  | "%" -> Node.Mod
  | "~" -> Node.Invert
  | "and" | "&&" -> Node.And
  | "or" | "||" -> Node.Or
  | "not" | "!" -> Node.Not
  | "!=" -> Node.NotEqual
  | "defined" -> Node.Defined
  | _ -> Node.Unknown

let build_ast_from_file file =
  let json = Yojson.Basic.from_file file in
  convert json

let run() =
  build_ast_from_file "./ruby_op.json"
