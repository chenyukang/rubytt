open Core.Std

type op =
  | Add
  | Sub
  | Mul
  | Div
  | Pow
  | Cmp
  | Match
  | NotMatch
  | Equal
  | Eq
  | Lt
  | Gt
  | BitAnd
  | BitOr
  | BitXor
  | In
  | LShift
  | RShift
  | Mod
  | Invert
  | And
  | Or
  | Not
  | LtE
  | GtE
  | NotEqual
  | Defined
  | Unknown

type node_info = {
  mutable path: string;
  mutable file: string;
  ss: int;
  ee: int;
}
and
  node_type =
  | Nil
  | If of node * node * node
  | Index of node
  | Kwd of string * node
  | Int of int
  | Float of float
  | String of string
  | Symbol of string
  | Void
  | Name of string * string (* Fixme *)
  | Block of node list
  | BinOp of op * node * node (* left op * right op *)
  | UnaryOp of op * node
  | While of node * node
  | Assign of node * node
  | Yield of node
  | Return of node
  | Try of node * node * node * node
and
  node = {
  info: node_info;
  ty: node_type;
  mutable parent: node option;
}

let rec get_ast_root node =
  match node.parent with
  | Some(p) -> get_ast_root p
  | _ -> node;;

let set_node_parent node p =
  node.parent <- Some(p)

let set_node_file node f =
  node.info.file <- f

let set_node_path node p =
  node.info.path <- p

let nil_node =
  {
    info = {path = ""; file = ""; ss = -1; ee = -1};
    ty = Nil;
    parent = None;
  }

let make_nil_node file s e =
  {
    info = {path = ""; file = file; ss = s; ee = e};
    ty = Nil;
    parent = None;
  }

let add_children parent children =
  List.iter children ~f:(fun a -> set_node_parent a parent) 

let make_block_node stmts file s e =
  let block = {
    info = {path = ""; file = file; ss = s; ee = e};
    ty = Block(stmts);
    parent = None;
  } in
  add_children block stmts;
  block

let make_bin_node op left right file s e =
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = BinOp(op, left, right);
    parent = None;
  }

let make_unary_node op operand file s e =
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = UnaryOp(op, operand);
    parent = None;
  }

let make_int_node value file s e =
  let v = Int.of_string value in
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Int(v);
    parent = None;
  }

let make_float_node value file s e =
  let v = Float.of_string value in
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Float(v);
    parent = None;
  }

let make_string_node str file s e =
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = String(str);
    parent = None;
  }

let make_symbol_node sym file s e =
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Symbol(sym);
    parent = None;
  }

let make_name_node id file s e =
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Name(id, ""); (* FIXME *)
    parent = None;
  }

let make_yield_node value file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Yield(value);
    parent = None;
  } in
  set_node_parent value node;
  node

let make_return_node value file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Return(value);
    parent = None;
  } in
  set_node_parent value node;
  node


let make_while_node test body file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e};
    ty = While(test, body);
    parent = None;
  } in
  add_children node [test; body];
  node

let make_assign_node target value file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Assign(target, value);
    parent = None;
  } in
  add_children node [target; value];
  node

let make_try_node body rescue orelse final file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Try(body, rescue, orelse, final);
    parent = None;
  } in
  add_children node [body; rescue; orelse; final];
  node

