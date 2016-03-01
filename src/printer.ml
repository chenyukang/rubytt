open Core.Std;;
open Node
open State

let op_to_str op =
  match op with
  | Add -> "Add" | Sub -> "Sub"
  | Mul -> "Mul" | Div -> "Div"
  | Pow -> "Pow" | Cmp -> "Cmp"
  | Match -> "Match" | NotMatch -> "NotMatch"
  | Equal -> "Equal" | Eq -> "Eq" | Lt -> "Lt" | Gt -> "Gt"
  | BitAnd -> "BitAnd" | BitOr -> "BitOr" | BitXor -> "BitXor"
  | In -> "In" | LShift -> "LShift" | RShift -> "RShift"
  | Mod -> "Mod" | Invert -> "Invert"
  | And -> "And" | Or -> "Or" | Not -> "Not"
  | LtE -> "LtE" | GtE -> "GtE"
  | NotEqual -> "NotEqual" | Defined -> "Defined"
  | Unknown -> "Unknown"

let rec node_to_str node depth =
  let str =
    match node.ty with
    | Nil -> "(Nil)"
    | Void -> "(Void)"
    | Int(v) -> "(Int " ^ Int.to_string v ^ ")"
    | Float(v) -> "(Float " ^ Float.to_string v ^ ")"
    | String(s) -> "(String " ^ s ^ ")"
    | Symbol(s) -> "(Symbol " ^ s ^ ")"
    | Control(s) -> "(Control " ^ s ^ ")"
    | Name(s, k) -> "(Name " ^ s ^ " " ^ (Node.name_ty_to_str k) ^ ")"
    | Kwd(kwd, node) -> "(Kwd " ^ kwd ^ (node_to_str node (depth + 1)) ^ ")"
    | Block(stmts) ->
      let s = ref "(Block " in
      List.iter stmts ~f:(fun x -> s := !s ^ node_to_str x (depth+1));
      !s ^ ")"
    | Assign(n1, n2) ->
      "(Assign " ^
      (node_to_str n1 (depth + 1)) ^
      (node_to_str n2 (depth + 1)) ^ ")"
    | BinOp(op, n1, n2) ->
      let ops = op_to_str op in
      "(BinOp:" ^ ops ^ " " ^
      node_to_str n1 (depth+1) ^
      node_to_str n2 (depth+1) ^ ")"
    | UnaryOp(op, n) ->
      let op = op_to_str op in
      "(UnaryOp:" ^ op ^ " " ^
      node_to_str n (depth+1) ^ ")"
    | While(n1, n2) ->
      "(While " ^
      node_to_str n1 (depth+1) ^
      node_to_str n2 (depth+1) ^ ")"
    | Yield(n1) ->
      "(Yield " ^
      node_to_str n1 (depth+1) ^ ")"
    | Return(n1) ->
      "(Return " ^
      node_to_str n1 (depth+1) ^ ")"
    | Attribute(n1, n2) ->
      "(Attribute " ^
      node_to_str n1 (depth+1) ^
      node_to_str n2 (depth+1) ^ ")"
    | If(n1, n2, n3) ->
      "(If " ^
      node_to_str n1 (depth+1) ^
      node_to_str n2 (depth+1) ^
      node_to_str n3 (depth+1) ^ ")"
    | Try(n1, n2, n3, n4) ->
      "(Try " ^
      node_to_str n1 (depth+1) ^
      node_to_str n2 (depth+1) ^
      node_to_str n3 (depth+1) ^
      node_to_str n4 (depth+1) ^ ")"
    | For(n1, n2, n3) ->
      "(For " ^
      node_to_str n1 (depth+1) ^
      node_to_str n2 (depth+1) ^
      node_to_str n3 (depth+1) ^ ")"
    | Subscript(name, slice) ->
      let res = ref "(Subscript " in
      res := !res ^ node_to_str name (depth+1);
      for i = 0 to (List.length slice - 1) do
        let n = node_to_str (List.nth_exn slice i) (depth+1) in
        res := !res ^ n;
      done;
      !res ^ ")"
    | Handler(ns, n1, n2, n3) ->
      let res = ref "(Handler " in
      for i = 0 to (List.length ns - 1) do
        let n = node_to_str (List.nth_exn ns i) (depth+1) in
        res := !res ^ n;
      done;
      !res ^
      (node_to_str n1 (depth+2)) ^
      (node_to_str n2 (depth+2)) ^
      (node_to_str n3 (depth+2))
    | Array(ns) ->
      let res = ref "(Array " in
      for i = 0 to (List.length ns - 1) do
        let v = node_to_str (List.nth_exn ns i) (depth+1) in
        res := !res ^ v
      done;
      !res ^ ")"
    | Dict(k, v) ->
      let res = ref "(Dict  " in
      for i = 0 to (List.length k - 1) do
        let _k = node_to_str (List.nth_exn k i) (depth + 1) in
        let _v = node_to_str (List.nth_exn v i) (depth + 2) in
        res := !res ^ _k ^ _v
      done;
      !res ^ ")"
    | Module(_, n1, n2, _) ->
      "(Module " ^
      node_to_str n1 (depth+1) ^
      node_to_str n2 (depth+2) ^ ")"
    | Class(n1, n2, n3, _, static) ->
      "(Class static: " ^
      (if static then "true" else "false" ) ^
      node_to_str n1 (depth+1) ^
      node_to_str n2 (depth+1) ^
      node_to_str n3 (depth+2) ^ ")"
    | Call(name, pos, _, _) ->
      let res = ref "(Call " in (
      match name.ty with
      | Name(s, _) -> res := !res ^ s
      | _ -> res := !res ^ node_to_str name (depth+1));
      if (List.length pos) <> 0 then (
        res := !res ^ nw (depth+1) ^ "(args: ";
        List.iter pos ~f:(fun x ->
            let a = node_to_str x (depth+2) in
            res := !res ^ a;
          );
        res := !res ^ ")";
      );
      !res ^ ")"
    | Func(info) ->
      let n = match info.name.ty with
        | Name(s, _) -> s
        | _ -> "__" in
      let res = ref ("(Func " ^ n) in
      if (List.length info.args) <> 0 then (
        res := !res ^ nw (depth+1) ^ "(args: ";
        List.iter info.args ~f:(fun x ->
            let a = node_to_str x (depth+2) in
            res := !res ^ a;
          );
        res := !res ^ ")"
      );
      if (List.length info.kw_ks) <> 0 then (
        res := !res ^ nw (depth+1) ^ "(kw: ";
        for i = 0 to (List.length info.kw_ks - 1) do
          let k = node_to_str (List.nth_exn info.kw_ks i) (depth+2) in
          let v = node_to_str (List.nth_exn info.kw_vs i) (depth+2) in
          res := !res ^ k ^ " --> " ^ v;
        done;
        res := !res ^ ")"
      );
      if (List.length info.after_rest) <> 0 then (
        res := !res ^ nw (depth+1) ^ "(after_rest: ";
        List.iter info.after_rest ~f:(fun x ->
            let a = node_to_str x (depth+2) in
            res := !res ^ a;
          );
        res := !res ^ ")";
      );
      res := !res ^ nw (depth+1) ^ "(body: " ^ node_to_str info.body (depth+2) ^ ")";
      !res ^ ")"
    | _ -> "other" in
  match depth with
  | 0 -> str | _ -> "\n" ^ (k_space depth) ^ str
and
  k_space n = String.init n ~f:(fun _ -> ' ')
and
  nw n = "\n" ^ k_space n

open Type
let print_table = ref true;;
let rec type_to_str ty depth =
  match ty.ty with
  | Int_ty -> "Int_ty"
  | Str_ty _  -> "Str_ty"
  | Bool_ty _ -> "Bool_ty"
  | List_ty(elem_ty, _, _) ->
    let elem_str = (type_to_str elem_ty 0) in
    Printf.sprintf "[%s]" elem_str
  | Class_ty(name, _, _) ->
    let name = Printf.sprintf "Class_ty: %s" name in
    if !print_table then
      name ^ (table_to_str ty.info.table (depth+1))
    else
      name
  | Module_ty(id, _) ->
    Printf.sprintf "Module_ty: %s" id
    ^ (table_to_str ty.info.table (depth+1))
  | Instance_ty(class_ty) -> (
      let class_name = classty_get_name class_ty in
      match class_name with
      | "?" -> "Unkown_ty"
      | "nil" -> "Nil_ty"
      | _ ->
        Printf.sprintf "Inst_ty: %s" class_name
    )
  | Union_ty(tys_table)  -> (
      let res = ref "{" in
      Hashtbl.iter tys_table ~f:(fun ~key:k ~data:_ ->
          let sep = if !res = "{" then "(" else "|(" in
          res := !res ^ sep ^ (type_to_str k 0) ^ ")");
      !res ^ "}"
    )
  | Fun_ty(info) -> (
      let defaults = ref "[" in
      List.iteri info.def_tys ~f:(fun i x ->
          let s = if i > 0 then " " else "" in
          defaults := !defaults ^ s ^ type_to_str x 0);
      defaults := !defaults ^ "]";
      print_table := false;
      let ret_str = type_to_str info.ret_ty 0 in
      print_table := true;
      Printf.sprintf "Func_ty: %s => %s" !defaults ret_str
    )
  | _ -> "unkown_type"

and
  table_to_str (state:Type.state_t) depth =
  let table = state.s_table in
  let res = ref "" in
  Hashtbl.iter table ~f:(fun ~key:name ~data:bindings ->
      (* avoid loop *)
      Printf.printf "now name: %s\n" name;
      if name <> "self" then (
        let final_ty = make_unions_from_bs bindings in
        let ty_str = type_to_str final_ty depth in
        let str = Printf.sprintf "bind: %s ty: %s" name ty_str in
        res := !res ^
               (match depth with
                | 0 -> str ^ "\n"
                | _ -> "\n" ^ (k_space (2 * depth)) ^ str);
      )
    );
  !res

