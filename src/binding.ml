
type kind =
  | ModuleK
  | ClassK
  | MethodK
  | ClassMethodK
  | AttributeK
  | ParameterK
  | ScopeK
  | VariableK
  | ConstK
and
  ('ty, 'node) binding = {
  node: 'node;
  qname: string;
  bfile: string;
  bty: 'ty;
  kind: kind;
  start: int;
  tail: int;
  body_start: int;
  body_end: int;
}
