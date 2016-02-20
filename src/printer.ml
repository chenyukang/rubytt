open Core.Std;;
open Node
open State

let op_to_str op =
  match op with
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Pow -> "Pow"
  | Cmp -> "Cmp"
  | Match -> "Match"
  | NotMatch -> "NotMatch"
  | Equal -> "Equal"
  | Eq -> "Eq"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | BitAnd -> "BitAnd"
  | BitOr -> "BitOr"
  | BitXor -> "BitXor"
  | In -> "In"
  | LShift -> "LShift"
  | RShift -> "RShift"
  | Mod -> "Mod"
  | Invert -> "Invert"
  | And -> "And"
  | Or -> "Or"
  | Not -> "Not"
  | LtE -> "LtE"
  | GtE -> "GtE"
  | NotEqual -> "NotEqual"
  | Defined -> "Defined"
  | Unknown -> "Unknown"

let rec node_to_str node depth =
  let str =
    match node.ty with
    | Nil -> "(Nil)"
    | Void -> "(Void)"
    | Kwd(kwd, node) ->
      "(Kwd " ^ kwd ^ (node_to_str node (depth + 1)) ^ ")"
    | Int(v) -> "(Int " ^ Int.to_string v ^ ")"
    | Float(v) -> "(Float " ^ Float.to_string v ^ ")"
    | String(s) -> "(String " ^ s ^ ")"
    | Symbol(s) -> "(Symbol " ^ s ^ ")"
    | Control(s) -> "(Control " ^ s ^ ")"
    | Name(s, _) -> "(Name " ^ s ^ ")"
    | Block(stmts) ->
      let s = ref "(Block " in
      List.iter stmts ~f:(fun x ->
          s := !s ^ node_to_str x (depth+1)
        );
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
    | Module(n1, n2, _) ->
      "(Module " ^
      node_to_str n1 (depth+1) ^
      node_to_str n2 (depth+2) ^ ")"
    | Class(n1, n2, n3, _, _) ->
      "(Class " ^
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
    | Func(name, args, _, kw_ks, kw_vs, after_rest, _, body, _) ->
      let n = match name.ty with
        | Name(s, _) -> s
        | _ -> "__" in
      let res = ref ("(Func " ^ n) in
      if (List.length args) <> 0 then (
        res := !res ^ nw (depth+1) ^ "(args: ";
        List.iter args ~f:(fun x ->
            let a = node_to_str x (depth+2) in
            res := !res ^ a;
          );
        res := !res ^ ")"
      );
      if (List.length kw_ks) <> 0 then (
        res := !res ^ nw (depth+1) ^ "(kw: ";
        for i = 0 to (List.length kw_ks - 1) do
          let k = node_to_str (List.nth_exn kw_ks i) (depth+2) in
          let v = node_to_str (List.nth_exn kw_vs i) (depth+2) in
          res := !res ^ k ^ " --> " ^ v;
        done;
        res := !res ^ ")"
      );
      if (List.length after_rest) <> 0 then (
        res := !res ^ nw (depth+1) ^ "(after_rest: ";
        List.iter after_rest ~f:(fun x ->
            let a = node_to_str x (depth+2) in
            res := !res ^ a;
          );
        res := !res ^ ")";
      );
      res := !res ^ nw (depth+1) ^
             "(body: " ^ node_to_str body (depth+2) ^ ")";
      !res ^ ")"
    | _ -> "other" in
  match depth with
  | 0 -> str
  | _ -> "\n" ^ (k_space depth) ^ str
and
  k_space n =
  String.init n ~f:(fun _ -> ' ')
and
  nw n =
  "\n" ^ k_space n


let table_to_str (state:Type.state_t) =
  let table = state.s_table in
  Hashtbl.iter table ~f:(fun ~key:k ~data:bindings ->
      List.iter bindings ~f:(fun b ->
          Printf.printf "bind: %s ty: %s\n" k (Type.type_to_str b.bind_ty)
        )
    );
  ""
