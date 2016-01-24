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

type name_ty =
  | Local
  | Instance
  | Class
  | Global

type node_info = {
  mutable path: string;
  mutable file: string;
  ss: int;
  ee: int;
}
and
  node_type =
  | Nil
  | Index of node
  | Kwd of string * node
  | Int of int
  | Float of float
  | String of string
  | Symbol of string
  | Control of string
  | Void
  | Name of string * name_ty
  | Block of node list
  | BinOp of op * node * node (* left op * right op *)
  | UnaryOp of op * node
  | While of node * node
  | Assign of node * node
  | Yield of node
  | Return of node
  | Attribute of node * node
  | Try of node * node * node * node
  | If of node * node * node
  | For of node * node * node
  | Regexp of node * node
  | Undef of node list
  | StrEmbed of string
  | Starred of node
  | Array of node list
  | Module of node * node * string
  | Class of node * node * node * string * bool
  | Handler of node list * node * node * node
  | Dict of node list * node list
  | Call of node * node list * node * node 
  | Func of node * node list * node list * node list * node list * node list *
            node * node * string
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

let make_kwd_node str value file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Kwd(str, value);
    parent = None;
  } in
  set_node_parent value node;
  node


let make_symbol_node sym file s e =
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Symbol(sym);
    parent = None;
  }

let make_void_node file s e =
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Void;
    parent = None;
  }

let make_control_node ty file s e =
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Control(ty);
    parent = None;
  }

let make_name_node id ty file s e =
  {
    info = {path=""; file = file; ss = s; ee = e};
    ty = Name(id, ty);
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

let make_if_node test body _else file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e};
    ty = If(test, body, _else);
    parent = None;
  } in
  add_children node [test; body; _else];
  node

let make_for_node target iter body file s e = 
  let node = {
    info = {path=""; file = file; ss = s; ee = e};
    ty = For(target, iter, body);
    parent = None;
  } in
  add_children node [target; iter; body];
  node

let make_regexp_node pat _end file s e =
  {
    info = {path = ""; file = file; ss = s; ee = e};
    ty = Regexp(pat, _end);
    parent = None;
  }

let make_strembed_node value file s e =
  {
    info = {path = ""; file = file; ss = s; ee = e};
    ty = StrEmbed(value);
    parent = None;
  }

let make_starred_node value file s e =
  let node = {
    info = {path = ""; file = file; ss = s; ee = e};
    ty = Starred(value);
    parent = None;
  } in
  set_node_parent value node;
  node

let make_array_node elems file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Array(elems);
    parent = None;
  } in
  add_children node elems;
  node

let make_attribute_node value attr file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Attribute(value, attr);
    parent = None;
  } in
  add_children node [value; attr];
  node

let make_undef_node targets file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Undef(targets);
    parent = None;
  } in
  add_children node targets;
  node

let make_module_node name body doc file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Module(name, body, doc);
    parent = None;
  } in
  add_children node [name; body];
  node

let make_class_node name super body doc static file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Class(name, super, body, doc, static);
    parent = None;
  } in
  add_children node [name; super; body];
  node

let make_handler_node exceps binder handler orelse file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Handler(exceps, binder, handler, orelse);
    parent = None;
  } in
  add_children node [binder; handler; orelse];
  add_children node exceps;
  node


let make_dict_node keys vals file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Dict(keys, vals);
    parent = None;
  } in
  add_children node keys;
  add_children node vals;
  node

let make_func_node binder positional defaults kw_ks kw_vs
    after_rest block_arg body doc file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Func(binder, positional, defaults, kw_ks, kw_vs,
              after_rest, block_arg, body, doc);
    parent = None;
  } in
  add_children node positional;
  add_children node defaults;
  add_children node after_rest;
  add_children node kw_ks;
  add_children node kw_vs;
  add_children node [binder; body; block_arg];
  node

let make_call_node func pos star block_arg file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Call(func, pos, star, block_arg);
    parent = None;
  } in
  add_children node pos;
  add_children node [func; star; block_arg];
  node
