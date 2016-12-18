
module NodeHashT =
  struct
    type t = Node.node_t
    let equal n1 n2 = Node.compare_node_t n1 n2 = 0
    let hash n = Node.node_t_hash n;
  end
module NodeHashtbl = Hashtbl.Make(NodeHashT);;


module NodeSetT =
  struct
    type t = Node.node_t
    let compare n1 n2 =
      Node.compare_node_t n1 n2
  end

module NodeSet = Set.Make(NodeSetT)

module StringSet = Set.Make(String)

(* module TypeSetT = *)
(*   struct *)
(*     type t = Type.type_t *)
(*     let compare t1 t2 = *)
(*       Type.compare_type_t t1 t2 *)
(*   end *)

(* module TypeSet = Set.Make(TypeSetT) *)

                         (* module TypeHashT = *)
(*   struct *)
(*     type t = type_t *)
(*     let equal t1 t2 = if compare_type_t t1 t2 = 0 then true else false *)
(*     let hash t = type_t_hash t; *)
(*   end *)
(* module TypeHashtbl = Hashtbl.Make(TypeHashT);; *)

(* let h = NodeHashtbl.create 17 in *)
(*     NodeHashtbl.add h Node.nil_node true;; *)
