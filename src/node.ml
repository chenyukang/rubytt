
type node_info = {
  path: string;
  file: string;
  ss: int;
  ee: int;
  parent: node;
}
and
  node =
  | Nil
  | IfNode of node_info * node * node * node
  | IndexNode of node_info * node
  | KwdNode of node_info * string * node


