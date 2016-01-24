open Core.Std
open Yojson
open Yojson.Basic.Util
open Yojson.Basic
open UnixLabels
open Node
open Printer

let convert_to_s json mem =
  let r = json |> member mem in
  match r with
  | `String(str) -> str
  | _ -> ""

let convert_to_i json mem =
  let elem = json |> member mem in
  match elem with
  | `Int(v) -> v
  | _ -> 0

let rec convert json =
  let ty = convert_to_s json "type" in
  let ss = convert_to_i json "start" in
  let ee = convert_to_i json "end" in
  let ff = convert_to_s json "filename" in 
  (* Printf.printf "now type: %s\n" ty; *)
  match ty with
  | "program" ->
    convert_elem json "body"
  | "block" ->
    let stmts = convert_list (json |> member "stmts") in
    make_block_node stmts ff ss ee
  | "int" ->
    let v = convert_to_s json "value" in
    make_int_node v ff ss ee  (* FIXME *)
  | "float" ->
    let v = convert_to_s json "value" in
    make_float_node v ff ss ee (* FIXME *)
  | "symbol" ->
    let sym = convert_to_s json "id" in
    make_symbol_node sym ff ss ee
  | "string" ->
    let str = convert_to_s json "id" in
    make_string_node str ff ss ee
  | "name" ->
    let id = convert_to_s json "id" in
    make_name_node id Node.Local ff ss ee (* FIXME *)
  | "attribute" ->
    let value = convert_elem json "value" in
    let attr = convert_elem json "attr" in
    make_attribute_node value attr ff ss ee
  | "undef" ->
    let targets = convert_list (json |> member "names") in
    make_undef_node targets ff ss ee
  | "keyword" ->
    let arg = convert_to_s json "arg" in
    let value = convert_elem json "value" in
    make_kwd_node arg value ff ss ee
  | "void" ->
    make_void_node ff ss ee
  | "break" | "retry" | "redo" | "continue" ->
    make_control_node ty ff ss ee
  | "unary" ->
    let op = convert_op (json |> member "op") in
    let operand = convert_elem json "operand" in
    make_unary_node op operand ff ss ee
  | "yield" ->
    let value = convert_elem json "value" in
    make_yield_node value ff ss ee
  | "return" ->
    let value = convert_elem json "value" in
    make_return_node value ff ss ee
  | "while" ->
    let test = convert_elem json "test" in
    let body = convert_elem json "body" in
    make_while_node test body ff ss ee
  | "if" ->
    let test = convert_elem json "test" in
    let body = convert_elem json "body" in
    let _else = convert_elem json "else" in
    make_if_node test body _else ff ss ee
  | "for" ->
    let target = convert_elem json "target" in
    let iter = convert_elem json "iter" in
    let body = convert_elem json "body" in
    make_for_node target iter body ff ss ee
  | "assign" ->
    let _var = convert_elem json "target" in
    let _val = convert_elem json "value" in
    make_assign_node _var _val ff ss ee
  | "begin" ->
    let body = convert_elem json "body" in
    let _rescue = convert_elem json "rescue" in
    let orelse = convert_elem json "else" in
    let final = convert_elem json "ensure" in
    make_try_node body _rescue orelse final ff ss ee
  | "regexp" ->
    let pat = convert_elem json "pattern" in
    let reg_end = convert_elem json "regexp_end" in
    make_regexp_node pat reg_end ff ss ee
  | "embexp" ->
    let value = convert_to_s json "value" in
    make_strembed_node value ff ss ee
  | "arg" ->
    let id = convert_to_s json "arg" in
    make_name_node id Node.Local ff ss ee
  | "star" ->
    let value = convert_elem json "value" in
    make_starred_node value ff ss ee
  | "cvar" ->
    let id = convert_to_s json "id" in
    make_name_node id Node.Class ff ss ee
  | "ivar" ->
    let id = convert_to_s json "id" in
    make_name_node id Node.Instance ff ss ee
  | "gvar" ->
    let id = convert_to_s json "id" in
    make_name_node id Node.Global ff ss ee
  | "dot2" | "dot3" ->
    let _fr = convert_elem json "from" in
    let _to = convert_elem json "to" in
    make_array_node [_fr; _to] ff ss ee
  | "array" ->
    let eles = convert_list (json |> member "elts") in
    make_array_node eles ff ss ee
  | "module" ->
    let name = convert_elem json "name" in
    let body = convert_elem json "body" in
    let doc  = convert_to_s json "doc" in
    (* Printf.printf "doc: %s\n" doc; *)
    make_module_node name body doc ff ss ee
  | "class" ->
    let name = convert_elem json "name" in
    let super = convert_elem json "super" in
    let body = convert_elem json "body" in
    let doc = convert_to_s json "doc" in
    let is_static = json |> member "static" |> to_bool in
    make_class_node name super body doc is_static ff ss ee
  | "rescue" ->
    let exceptions = convert_list (json |> member "exceptions") in
    let binder = convert_elem json "binder" in
    let handler = convert_elem json "handler" in
    let orelse = convert_elem json "else" in
    make_handler_node exceptions binder handler orelse ff ss ee
  | "def" | "lambda" ->
    let binder = convert_elem json "name" in
    let body = convert_elem json "body" in
    let params = json |> member "params" in
    let positional = convert_list (params |> member "positional") in
    let defaults = convert_list (params |> member "defaults") in
    let after_rest = convert_list (params |> member "after_rest") in
    let block_arg = convert_elem params "blockarg" in
    let doc = convert_to_s json "doc" in
    make_func_node binder positional defaults after_rest block_arg body doc ff ss ee
  | "args" -> (
      let pos = (json |> member "positional") in
      match pos with
      | `Null -> (
          let elts = convert_list (json |> member "star") in
          make_array_node elts ff ss ee
        )
      | _ -> (
          let arr = convert_list pos in
          make_array_node arr ff ss ee
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
       | `Null -> ();
       | _ -> failwith "type error convert hash\n" );
      make_dict_node !keys !vals ff ss ee
    )
  | "binary" -> (
    let l = convert_elem json "left" in
    let r = convert_elem json "right" in
    let op = convert_op (json |> member "op") in
    match op with
    | LtE ->
      let lt = make_bin_node Node.Lt l r ff ss ee in
      let eq = make_bin_node Node.Eq l r ff ss ee in
      make_bin_node Or lt eq ff ss ee
    | GtE ->
      let gt = make_bin_node Node.Gt l r ff ss ee in
      let eq = make_bin_node Node.Eq l r ff ss ee in
      make_bin_node Node.Or gt eq ff ss ee
    | NotEqual ->
      let eq = make_bin_node Node.Equal l r ff ss ee in
      make_unary_node Node.Not eq ff ss ee
    | NotMatch ->
      let eq = make_bin_node Match l r ff ss ee in
      make_unary_node Node.Not eq ff ss ee
    | _ -> make_bin_node op l r ff ss ee
    )
  | _ -> (* Printf.printf "\nhere\n"; *) nil_node
and
  convert_elem json mem =
  let elem = json |> member mem in
  match elem with
  | `Null -> nil_node
  | _ -> convert elem
and
  convert_list stmts =
  match stmts with
  | `List(v) ->
    List.map v ~f:(fun e -> convert e)
  | `Null -> []
  | _ -> failwith "type error in convert_list\n"; []
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

