open Core.Std
open Typestack
open State
open Node

type
  state_t = (type_t, binding_ty) state
and
  ty_info = {
  file: string;
  mutated: bool;
  table: state_t;
}
and
  fun_info = {
  fun_node: node_t;
  class_ty: type_t option;
  self_ty: type_t option;
  env: state_t option;
  def_tys: type_t list;
  ret_ty: type_t;
  is_class: bool
}
and
  bool_value =
  | True
  | False
  | Undecided
and ty =
  | Bool_ty of bool_value * state_t option * state_t option
  | Int_ty
  | Str_ty of string
  | Sym_ty of string
  | Float_ty
  | Instance_ty of type_t
  (* name * instance_type * superclass *)
  | Class_ty of string * type_t option * type_t option
  | Module_ty of string * string
  | Union_ty of (type_t, bool) Hashtbl.t
  | Tuple_ty of type_t list
  (* elem_ty * positional * values *)
  | List_ty of type_t * type_t list * node_t list
  | Fun_ty of fun_info
and
  type_t = {
  mutable info: ty_info;
  mutable ty: ty;
}
and kind =
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
  binding_ty = {
  node: node_t;
  refs: (Node.node_t, bool) Hashtbl.t;
  qname: string;
  bind_file: string;
  bind_ty: type_t;
  kind: kind;
  start: int;
  tail: int;
  body_start: int;
  body_end: int;
}

let global_table =
  State.new_state ~parent:None State.Global

let type_stack = TypeStack.empty;;

let is_mutated (t: type_t) = t.info.mutated

let set_mutated t m = t.info <- {t.info with mutated = m}

let set_file t f = t.info <- {t.info with file = f}

let set_table (t: type_t) (table: state_t) =
  t.info <- {t.info with table = table}


let is_undecided_bool t =
  match t.ty with
  | Bool_ty(bv, _, _) ->
    (match bv with | Undecided -> true | _ -> false)
  | _ -> false

let is_num_type t =
  match t.ty with
  | Int_ty | Float_ty -> true | _ -> false

let is_str_type t =
  match t.ty with
  | Str_ty _ -> true | _ -> false

let rec type_equal ty1 ty2 =
  match ty1.ty, ty2.ty with
  | Int_ty, Int_ty
  | Str_ty _, Str_ty _
  | Float_ty, Float_ty
  | Bool_ty _, Bool_ty _ -> true
  | Class_ty _, Class_ty _ -> phys_equal ty1 ty2
  | Module_ty(_, f1), Module_ty(_, f2) -> (
      f1 = f2 && (phys_equal ty1 ty2)
    )
  | Instance_ty(t1), Instance_ty(t2) ->
      (type_equal t1 t2)
  | _, _ -> phys_equal ty1 ty2


let new_ty_info() =
  {
    file = ""; mutated = false;
    table = (State.new_state ~parent:(Some global_table) State.Global);
  }

let new_bool_type ?(v=Undecided) ?(s1=None) ?(s2=None) () =
  { info = new_ty_info();
    ty = Bool_ty(v, s1, s2);
  }

let bool_set_value b v =
  match b.ty with
  | Bool_ty(_, s1, s2) -> {info = b.info; ty = Bool_ty(v, s1, s2)}
  | _ -> failwith "bool_set_value"

let bool_set_s1 b s1 =
  match b.ty with
  | Bool_ty(v, _, s2) -> b.ty <- Bool_ty(v, s1, s2)
  | _ -> failwith "bool_set_s1"

let bool_set_s2 b s2 =
  match b.ty with
  | Bool_ty(v, s1, _) -> b.ty <- Bool_ty(v, s1, s2)
  | _ -> failwith "bool_set_s2"

let bool_swap b =
  match b.ty with
  | Bool_ty(v, s1, s2) ->
    {b with ty = Bool_ty(v, s2, s1)}
  | _ -> failwith "bool_swap"

let new_int_type () =
  {
    info = new_ty_info();
    ty = Int_ty;
  }

let new_float_type () =
  {
    info = new_ty_info();
    ty = Float_ty;
  }

let new_str_type ?(value="") () =
  {
    info = new_ty_info();
    ty = Str_ty(value);
  }

let new_sym_type ?(name="") () =
  {
    info = new_ty_info();
    ty = Sym_ty(name);
  }

let classty_set_name c name =
  match c.ty with
  | Class_ty(_name, canon, super) -> c.ty <- Class_ty(name, canon, super)
  | _ -> failwith "classty_set_name"

let classty_set_canon c canon =
  match c.ty with
  | Class_ty(name, _canon, super) -> c.ty <- Class_ty(name, canon, super)
  | _ -> failwith "classty_set_canon"

let classty_add_super c super =
  match c.ty with
  | Class_ty(name, canon, _) -> c.ty <- Class_ty(name, canon, super)
  | _ -> failwith "classty_add_super"

let cassty_get_canon c =
  match c.ty with
  | Class_ty(_, canon, _) -> canon
  | _ -> None

let new_class_type name parent ?(super = None) () =
  let ret = { info = new_ty_info();
              ty = Class_ty(name, None, None);
            } in
  let state = (State.new_state ~parent:parent State.Class) in
  set_table ret state;
  State.set_ttype state (Some ret);
  let path = match parent with
    | Some(p) -> State.extend_path p name "::"
    | _ -> name in
  State.set_path state path;
  (match super with
   | Some(s) -> (
       classty_add_super ret super;
       State.set_supers state (Some s.info.table) (* Fixme *)
     )
   | _ -> ());
  ret

let is_class_ty ty =
  match ty.ty with
  | Class_ty(_) -> true | _ -> false

let new_module_type name file parent =
  let ret = { info = new_ty_info();
              ty = Module_ty(name, file); } in
  let state = (State.new_state ~parent:parent State.Module) in
  set_table ret state;
  let path = match parent with
    |Some(p) ->  (if String.length p.path > 0 then p.path ^ "::" ^ name else name)
    |_ -> name in
  State.set_path state path;
  State.set_ttype state (Some ret);
  ret

let new_instance_type class_ty =
  {
    info = new_ty_info();
    ty = Instance_ty(class_ty);
  }

let int_ty =
  new_int_type()

let str_ty =
  new_str_type()

let float_ty =
  new_float_type()

let bool_ty =
  new_bool_type()

let cont_ty =
  let class_ty = new_class_type "nil" None ~super:None () in
  new_instance_type class_ty

let unkown_ty =
  let class_ty = new_class_type "?" None ~super:None () in
  new_instance_type class_ty


let rec union_ty_add_ty u t =
  match t.ty with
  | Union_ty(_t) ->
    Hashtbl.iter _t ~f:(fun ~key:k ~data:_ -> ignore(union_ty_add_ty u k))
  | _ -> (
      match u.ty with
      | Union_ty(table) ->
        ignore(Hashtbl.add table ~key:t ~data:true);
      | _ -> failwith "union_ty_add_ty error ty"
    )

let new_union_type ?(elems = []) () =
  let res = {
    info = new_ty_info();
    ty = Union_ty(Hashtbl.Poly.create ());
  } in
  List.iter elems ~f:(fun e -> union_ty_add_ty res e);
  res

let union_ty_is_empty t =
  match t.ty with
  | Union_ty(t) -> Hashtbl.length t = 0
  | _ -> false


let new_tuple_type elem_tys =
  {
    info = new_ty_info();
    ty = Tuple_ty(elem_tys)
  }

let new_binding node ttype kind =
  {
    qname = ttype.info.table.path;
    kind = kind;
    node = node;
    bind_file = node.info.file;
    bind_ty = ttype;
    start = 0;
    tail = 0;
    body_start = 0;
    body_end = 0;
    refs = Hashtbl.Poly.create ();
  }

let binding_add_ref binding node =
  Hashtbl.add_exn binding.refs ~key:node ~data:true

let bind_equal a b =
  (a.start = b.start && a.tail = b.tail && a.bind_file = b.bind_file)


let union_ty_remove u t =
  match u.ty with
  | Union_ty(_t) -> (
      let new_t = Hashtbl.copy _t in
      Hashtbl.remove new_t t;
      {
        info = new_ty_info();
        ty = Union_ty(new_t);
      }
    )
  | _ -> (
      if type_equal u t then unkown_ty
      else u
    )

let union_ty u v =
  match (type_equal u v) with
  | true -> u
  | _ when (type_equal u unkown_ty) -> v
  | _ when (type_equal v unkown_ty) -> u
  | _ when (type_equal u cont_ty) -> v
  | _ when (type_equal v cont_ty) -> u
  | _ -> new_union_type ~elems:[u; v] ()

let make_unions_from_bs bs =
  let res = ref unkown_ty in
  List.iter bs ~f:(fun b -> res := union_ty !res b.bind_ty );
  !res

let make_unions types =
  let res = ref unkown_ty in
  List.iter types ~f:(fun t -> res := union_ty !res t);
  !res

let new_list_type ?(ty = unkown_ty) () =
  {
    info = new_ty_info();
    ty = List_ty(ty, [], [])
  }

let list_ty_elem_ty list_ty =
  match list_ty.ty with
  | List_ty(base, _, _) -> base
  | _ -> failwith "list_ty_elem_ty type error"

let list_ty_add list_ty elem_ty =
  match list_ty.ty with
  | List_ty(base, ty_list, values) -> (
      let new_base = union_ty base elem_ty in
      list_ty.ty <- List_ty(new_base, ty_list @ [elem_ty], values)
    )
  | _ -> failwith "list_ty_add type error"


let is_list_ty ty =
  match ty.ty with
  | List_ty(_) -> true
  | _ -> false

let get_subscript_ty vt st =
  if type_equal vt unkown_ty then unkown_ty
  else (
    match vt.ty with
    | List_ty _ -> (
        if List.length st = 1 && is_num_type (List.nth_exn st 0) then
          list_ty_elem_ty vt
        else (
          (* dot2 or dot3 *)
          if List.length st > 1 then vt else unkown_ty
        )
      )
    | _ -> unkown_ty)       (* fixme *)


let new_fun_ty func env =
  {
    info = new_ty_info();
    ty = Fun_ty({fun_node = func; class_ty = None; self_ty = None;
                 env = env;  def_tys = []; ret_ty = unkown_ty;
                 is_class = false});
  }

let fun_ty_set_def_tys ty args_ty =
  match ty.ty with
  | Fun_ty(info) ->
      ty.ty <- Fun_ty({info with def_tys = args_ty})
  | _ -> failwith "fun_ty_set_defaults_ty error type"

let fun_ty_set_ret_ty ty ret_ty =
  match ty.ty with
  | Fun_ty(info) ->
    ty.ty <- Fun_ty({info with ret_ty = ret_ty})
  | _ -> failwith "fun_ty_set_ret_ty error type"

let fun_ty_set_class_ty ty cls_ty =
  match ty.ty with
  | Fun_ty(info) ->
    ty.ty <- Fun_ty({info with class_ty = cls_ty})
  | _ -> failwith "fun_ty_set_class_ty error type"
           
let fun_ty_info ty =
  match ty.ty with
  | Fun_ty(info) -> info
  | _ -> failwith "fun_ty_info error type"

let compare_type_t t1 t2=
  if type_equal t1 t2 then 0
  else
    -1

let type_t_of_sexp s =
  unkown_ty

let sexp_of_type_t ty =
  Int.sexp_of_t 1

let ty_of_state state =
  match (state.t_type) with
  | Some(ty) -> ty
  | None -> unkown_ty
