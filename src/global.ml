open Core.Std

module TypeSet : sig
  type t = Type.type_t
  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = Type.type_t with sexp
    (* use to compare fun_ty *)
    let compare t1 t2 =
      Type.compare_type_t t1 t2
  end
  include T
  include Comparable.Make(T)
end

module NodeSet : sig
  type t = Node.node_t
  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = Node.node_t with sexp
    let compare t1 t2 =
      Node.compare_node_t t1 t2
  end
  include T
  include Comparable.Make(T)
end

module TypeHash : sig
  type t = Type.type_t
  include Hashable.S with type t := t
end = struct
    module T = struct
      type t = Type.type_t with sexp, compare
      let hash t = Type.type_t_hash t
    end
    include T
    include Hashable.Make(T)
end

let refs:(Node.node_t, Type.binding_ty list) Hashtbl.t =
  Hashtbl.create ~hashable:Node.NodeHash.hashable ();;

let resolved = ref NodeSet.Set.empty;;
let unresolved = ref NodeSet.Set.empty;;
let callstack = ref NodeSet.Set.empty;;
let uncalled = ref TypeSet.Set.empty;;
let bindings:(Type.binding_ty list ref) = ref [];;

let clear() =
  Node.lambda_coutner := 0;
  State.state_clear Type.global_table;
  Hashtbl.clear refs;
  resolved := NodeSet.Set.empty;
  unresolved := NodeSet.Set.empty;
  callstack := NodeSet.Set.empty;
  uncalled := TypeSet.Set.empty


let print_size() =
  Printf.printf "resolve: %d\n%!" (NodeSet.Set.length !resolved);
  Printf.printf "unresolve: %d\n%!" (NodeSet.Set.length !unresolved);
  Printf.printf "callstack: %d\n%!" (NodeSet.Set.length !callstack);
  Printf.printf "uncalled: %d\n%!" (TypeSet.Set.length !uncalled)

let put_refs node bs =
  let bind = Hashtbl.find refs node in
  match bind with
  | None ->  (
      ignore(Hashtbl.add_exn refs ~key:node ~data:bs)
    )
  | Some(v) -> (
      List.iter bs ~f:(fun b -> Type.binding_add_ref b node);
      ignore(Hashtbl.replace refs ~key:node ~data:(v @ bs));
    )

let set_resolve node =
  resolved := NodeSet.Set.add !resolved node;
  unresolved := NodeSet.Set.remove !unresolved node

let set_unresolve node =
  unresolved := NodeSet.Set.add !unresolved node

let set_uncalled ty =
  uncalled := TypeSet.Set.add !uncalled ty

let set_called ty =
  uncalled := TypeSet.Set.remove !uncalled ty

let push_call call =
  callstack := NodeSet.Set.add !callstack call

let contains_call call =
  NodeSet.Set.mem !callstack call

let register_bind bind =
  bindings := !bindings @ [bind]
