open Def;;

module TypeSetT =
  struct
    type t = Type.type_t
    let compare t1 t2 =
      Type.compare_type_t t1 t2
  end

module TypeSet = Set.Make(TypeSetT)

(* module TypeSet : sig *)
(*   type t = Type.type_t *)
(*   include Comparable.S with type t := t *)
(* end = struct *)
(*   module T = struct *)
(*     type t = Type.type_t with sexp *)
(*     (\* use to compare fun_ty *\) *)
(*     let compare t1 t2 = Type.compare_type_t t1 t2 *)
(*   end *)
(*   include T *)
(*   include Comparable.Make(T) *)
(* end *)


let refs: (Type.binding_ty list) NodeHashtbl.t = NodeHashtbl.create 1;;

let resolved = ref NodeSet.empty;;
let unresolved = ref NodeSet.empty;;
let callstack = ref NodeSet.empty;;
let uncalled = ref TypeSet.empty;;
let bindings:(Type.binding_ty list ref) = ref [];;
let loaded_files = ref StringSet.empty;;

let clear() =
  Node.lambda_counter := 0;
  State.state_clear Type.global_table;
  NodeHashtbl.clear refs;
  loaded_files := StringSet.empty;
  resolved := NodeSet.empty;
  unresolved := NodeSet.empty;
  callstack := NodeSet.empty;
  uncalled := TypeSet.empty


let print_size() =
  Printf.printf "resolve: %d\n%!" (NodeSet.cardinal !resolved);
  Printf.printf "unresolve: %d\n%!" (NodeSet.cardinal !unresolved);
  Printf.printf "callstack: %d\n%!" (NodeSet.cardinal !callstack);
  Printf.printf "uncalled: %d\n%!" (TypeSet.cardinal !uncalled);
  Printf.printf "loaded_files: %d\n%!" (StringSet.cardinal !loaded_files);
  Printf.printf "refs: %d\n%!" (NodeHashtbl.length refs)

let put_refs node bs =
  try
    let v = NodeHashtbl.find refs node in
    List.iter (fun b -> Type.binding_add_ref b node) bs;
    NodeHashtbl.replace refs node (v @ bs);
  with
  | Not_found -> NodeHashtbl.add refs node bs

let set_resolve node =
  resolved := NodeSet.add node !resolved;
  unresolved := NodeSet.remove node !unresolved

let set_unresolve node =
  unresolved := NodeSet.add node !unresolved

let set_uncalled ty =
  uncalled := TypeSet.add ty !uncalled

let set_called ty =
  uncalled := TypeSet.remove ty !uncalled

let push_call call =
  callstack := NodeSet.add call !callstack

let contains_call call =
  NodeSet.mem call !callstack

let register_bind bind =
  bindings := !bindings @ [bind]

let set_load_file (file: string) =
  loaded_files := StringSet.add file !loaded_files
