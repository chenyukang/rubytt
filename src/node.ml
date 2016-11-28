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
type fun_node_info = {
  locator: node_t;
  name: node_t;
  args: node_t list;
  defaults: node_t list;
  kw_ks: node_t list;
  kw_vs: node_t list;
  after_rest: node_t list;
  block_arg: node_t;
  body: node_t;
  doc: string;
  is_lambda: bool
}
and
  node_type =
  | Nil
  | Index of node_t
  | Kwd of string * node_t
  | Int of int
  | Float of float
  | String of string
  | Symbol of string
  | Control of string
  | Void
  | Name of string * name_ty
  | Block of node_t list
  | BinOp of op * node_t * node_t (* left op * right op *)
  | UnaryOp of op * node_t
  | While of node_t * node_t
  | Assign of node_t * node_t
  | Yield of node_t
  | Return of node_t
  | Attribute of node_t * node_t
  | Try of node_t * node_t * node_t * node_t
  | If of node_t * node_t * node_t
  | For of node_t * node_t * node_t
  | Regexp of node_t * node_t
  | Undef of node_t list
  | StrEmbed of node_t
  | Starred of node_t
  | Array of node_t list
  | Module of node_t * node_t * node_t * string
  | Subscript of node_t * node_t list
  | Class of node_t * node_t * node_t * string * bool
  | Handler of node_t list * node_t * node_t * node_t
  | Dict of node_t list * node_t list
  | Call of node_t * node_t list * node_t * node_t
  | Func of fun_node_info

and
  node_t = {
  info: node_info;
  ty: node_type;
  mutable parent: node_t option;
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

let is_nil node =
  match node.ty with
  | Nil -> true  | _ -> false

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

let symbol_to_str sym =
  match sym.ty with | Symbol(s) -> s | _ -> ""

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

let is_name node =
  match node.ty with | Name(_) -> true | _ -> false

let name_is_global node =
  match node.ty with
  | Name(_, Global) -> true
  | _ -> false

let is_instance_var node =
  match node.ty with
  | Name(_, k) -> k = Instance
  | _ -> false

let name_node_id n =
  match n.ty with
  | Name(s, _) -> s
  | _ -> (* failwith "error node type for name_id" *) "unkown_name_id"

let name_node_is_globalvar n =
  match n.ty with
  | Name(_, t) -> t = Global
  | _ -> false

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

let try_attr_to_name attr =
    match attr.ty with
    | Attribute(_, attr) -> attr
    | Name _ | Nil -> attr
    | _ -> failwith "error type try_attr_to_name other"

let attr_id attr =
  let name = try_attr_to_name attr in
  name_node_id name

let is_attr node = match node.ty with | Attribute(_) -> true |_ -> false

let attr_target node =
  match node.ty with
  | Attribute(target, _) -> target
  | _ -> failwith "error type attr_target"

let attr_attr node =
    match node.ty with
  | Attribute(_, attr) -> attr
  | _ -> failwith "error type attr_attr"


let make_undef_node targets file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Undef(targets);
    parent = None;
  } in
  add_children node targets;
  node

let make_subscript_node name slice file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Subscript(name, slice);
    parent = None;
  } in
  set_node_parent name node;
  add_children node slice;
  node

let make_module_node locator body doc file s e =
  let name = try_attr_to_name locator in
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Module(locator, name, body, doc);
    parent = None;
  } in
  add_children node [locator; body];
  node

let make_class_node locator super body doc static file s e =
  let name = try_attr_to_name locator in
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
  add_children node ([binder; handler; orelse] @ exceps);
  node


let make_dict_node keys vals file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Dict(keys, vals);
    parent = None;
  } in
  add_children node (keys @ vals);
  node

let dict_node_get node key =
  match node.ty with
  | Dict(keys, values) -> (
      let nth = List.findi keys ~f:(fun i a ->
          match a.ty with
          | Name(key, _) -> true
          | _ -> false
        ) in
      match nth with
      | Some(pos, _) -> (
          let v = List.nth_exn values pos in
          match v.ty with
          | String(s) -> Some(s)
          | _ -> None
        )
      | _ -> None
    )
  | _ -> None


let lambda_counter = ref 0;;
let gen_lambda_name() =
  incr lambda_counter;
  Printf.sprintf "lambda%%%d" !lambda_counter

let make_func_node locator positional defaults kw_ks kw_vs
    after_rest block_arg body doc file s e =
  let is_lambda = is_nil locator in
  let loc = if is_nil locator then
      let gen_name = gen_lambda_name() in
      make_name_node gen_name Local file 0 0
    else
      locator in
  let name = try_attr_to_name loc in
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Func({locator = loc; name = name; args = positional;
               defaults = defaults; kw_ks = kw_ks;  kw_vs = kw_vs;
               after_rest = after_rest; block_arg = block_arg;
               body = body;  doc = doc;  is_lambda = is_lambda});
    parent = None;
  } in
  add_children node (positional @ defaults @ after_rest @ kw_ks @ kw_vs);
  add_children node [locator; body; block_arg];
  node

let func_node_name node =
  match node.ty with
  | Func(info) -> name_node_id info.name
  | _ -> "unknown"

let func_node_info node =
  match node.ty with
  | Func(info) -> info
  | _ -> failwith "func_node_info error type"

let is_lambda node =
  match node.ty with
  | Func(info) -> (
      let name = func_node_name node in
      (String.substr_index name "lambda%") = Some(0)
    )
  | _ -> false

let make_call_node func pos star block_arg file s e =
  let node = {
    info = {path=""; file = file; ss = s; ee = e };
    ty = Call(func, pos, star, block_arg);
    parent = None;
  } in
  add_children node ([func; star; block_arg] @ pos);
  node


let compare_node_t n1 n2 =
  let f1 = n1.info in
  let f2 = n2.info in
  if f1.file <> f2.file then
    String.compare f1.file f2.file
  else (
    if f1.ss <> f2.ss then Int.compare f1.ss f2.ss
    else Int.compare f1.ee f2.ee
  )

let node_t_of_sexp s =
  nil_node

let sexp_of_node_t ty =
  Int.sexp_of_t 1

let node_t_hash node =
  (String.hash node.info.file) lxor (String.hash node.info.path) lxor
  (Int.hash node.info.ss) lxor (Int.hash node.info.ee)

let name_ty_to_str t =
  match t with
  | Local -> "Local"
  | Instance -> "Instance"
  | Class -> "Class"
  | Global -> "Global"

module NodeHash : sig
  type t = node_t
  include Hashable.S with type t := t
end = struct
    module T = struct
      type t = node_t with sexp, compare
      let hash t = node_t_hash t
    end
    include T
    include Hashable.Make(T)
end
