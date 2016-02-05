
type state_type =
    Class
  | Instance
  | Function
  | Module
  | Global
  | Scope


type state = {
  parent: state;
  supers: state;
  s_type: state_type;
  (* t_type: Type.type_t; *)
  mutable path: string;
}
