open Core.Std
open Yojson
open Yojson.Basic.Util
open Yojson.Basic
open UnixLabels
open Node
open Printer

let convert_to_s json mem =
  let r = json |> member mem |> to_string in
  let x = String.drop_prefix r 1 in
  String.drop_suffix x 1

let convert_to_i json mem =
  json |> member mem |> to_int

let rec convert json =
  let ty = convert_to_s json "type" in
  let ss = convert_to_i json "start" in
  let ee = convert_to_i json "end" in
  (* Printf.printf "now type: %s" ty; *)
  match ty with
  | "program" ->
    convert_elem json "body"
  | "block" ->
    let stmts = convert_list (json |> member "stmts") in
    make_block_node stmts "file" ss ee
  | "int" ->
    let v = convert_to_s json "value" in
    make_int_node v "file" ss ee  (* FIXME *)
  | "float" ->
    let v = convert_to_s json "value" in
    make_float_node v "file" ss ee (* FIXME *)
  | "symbol" ->
    let sym = convert_to_s json "id" in
    make_symbol_node sym "file" ss ee
  | "string" ->
    let str = convert_to_s json "id" in
    (* Printf.printf "str: %s\n" str; *)
    make_string_node str "file" ss ee
  | "name" ->
    let id = convert_to_s json "id" in
    make_name_node id Node.Local "file" ss ee (* FIXME *)
  | "attribute" ->
    let value = convert_elem json "value" in
    let attr = convert_elem json "attr" in
    make_attribute_node value attr "file" ss ee
  | "undef" ->
    let targets = convert_list (json |> member "names") in
    make_undef_node targets "file" ss ee
  | "keyword" ->
    let arg = convert_to_s json "arg" in
    let value = convert_elem json "value" in
    make_kwd_node arg value "file" ss ee
  | "void" ->
    make_void_node "file" ss ee
  | "break" | "retry" | "redo" | "continue" ->
    make_control_node ty "file" ss ee
  | "unary" ->
    let op = convert_op (json |> member "op") in
    let operand = convert_elem json "operand" in
    make_unary_node op operand "file" ss ee
  | "yield" ->
    let value = convert_elem json "value" in
    make_yield_node value "file" ss ee
  | "return" ->
    let value = convert_elem json "value" in
    make_return_node value "file" ss ee
  | "while" ->
    let test = convert_elem json "test" in
    let body = convert_elem json "body" in
    make_while_node test body "file" ss ee
  | "if" ->
    let test = convert_elem json "test" in
    let body = convert_elem json "body" in
    let _else = convert_elem json "else" in
    make_if_node test body _else "file" ss ee
  | "for" ->
    let target = convert_elem json "target" in
    let iter = convert_elem json "iter" in
    let body = convert_elem json "body" in
    make_for_node target iter body "file" ss ee
  | "assign" ->
    let _var = convert_elem json "target" in
    let _val = convert_elem json "value" in
    make_assign_node _var _val "file" ss ee
  | "begin" ->
    let body = convert_elem json "body" in
    let _rescue = convert_elem json "rescue" in
    let orelse = convert_elem json "else" in
    let final = convert_elem json "ensure" in
    make_try_node body _rescue orelse final "file" ss ee
  | "regexp" ->
    let pat = convert_elem json "pattern" in
    let reg_end = convert_elem json "regexp_end" in
    make_regexp_node pat reg_end "file" ss ee
  | "embexp" ->
    let value = convert_to_s json "value" in
    make_strembed_node value "file" ss ee
  | "arg" ->
    let id = convert_to_s json "arg" in
    make_name_node id Node.Local "file" ss ee
  | "star" ->
    let value = convert_elem json "value" in
    make_starred_node value "file" ss ee
  | "cvar" ->
    let id = convert_to_s json "id" in
    make_name_node id Node.Class "file" ss ee
  | "ivar" ->
    let id = convert_to_s json "id" in
    make_name_node id Node.Instance "file" ss ee
  | "gvar" ->
    let id = convert_to_s json "id" in
    make_name_node id Node.Global "file" ss ee
  | "dot2" | "dot3" ->
    let _fr = convert_elem json "from" in
    let _to = convert_elem json "to" in
    make_array_node [_fr; _to] "file" ss ee
  | "class" ->
    let name = convert_elem json "name" in
    let super = convert_elem json "super" in
    let body = convert_elem json "body" in
    let doc = convert_elem json "doc" in
    let is_static = json |> member "static" |> to_bool in
    make_class_node name super body doc is_static "file" ss ee
  | "rescue" ->
    let exceptions = convert_list (json |> member "exceptions") in
    let binder = convert_elem json "binder" in
    let handler = convert_elem json "handler" in
    let orelse = convert_elem json "else" in
    make_handler_node exceptions binder handler orelse "file" ss ee
  | "args" -> (
      let pos = (json |> member "positional") in
      match pos with
      | `Null -> (
          let elts = convert_list (json |> member "star") in
          make_array_node elts "file" ss ee
        )
      | _ -> (
          let arr = convert_list pos in
          make_array_node arr "file" ss ee
        )
    )
  | "hash" -> (
      let keys = ref [] in
      let vals = ref [] in
      let entries = json |> member "entries" in
      (match entries with
       | `List(v) ->
           List.iter v ~f:(fun e ->
               let _k = convert_elem e "key" in
               let _v = convert_elem e "value" in
               keys := !keys @ [_k];
               vals := !vals @ [_v];
           )
       | _ -> Printf.printf ("type error convert hash\n") );
      make_dict_node !keys !vals "file" ss ee
    )
  | "binary" -> (
    let l = convert_elem json "left" in
    let r = convert_elem json "right" in
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
  convert_elem json mem =
  convert (json |> member mem)
and
  convert_list stmts =
  match stmts with
  | `List(v) ->
    List.map v ~f:(fun e -> convert e)
  | _ -> Printf.printf "type error in convert_list\n"; []
and
  convert_op op =
  let o = convert_to_s op "name" in
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

let parse_file file =
  build_ast_from_file file

let run() =
  let ast = build_ast_from_file "./ruby.json" in
  let str = node_to_str ast 0 in
  Printf.printf "%s\n" str

