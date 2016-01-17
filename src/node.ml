
type node_info = {
  mutable path: string;
  mutable file: string;
  ss: int;
  ee: int;
}
and
  node_type =
  | Nil
  | IfNode of node * node * node
  | IndexNode of node
  | KwdNode of string * node
  | Int of int
  | String of string
  | Symbol of string
  | Void
  | Name of string * string (* Fixme *)
  | Block of node list
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
  List.iter (fun a -> set_node_parent a parent) children

let make_block_node stmts file s e =
  let block = {
    info = {path = ""; file = file; ss = s; ee = e};
    ty = Block(stmts);
    parent = None;
  } in
  add_children block stmts;
  block




