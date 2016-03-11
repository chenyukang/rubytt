open Core.Std;;
open Node
open State
open Type

let class_hash = Hashtbl.Poly.create();;
let node_to_dot_str (state:Type.state_t) =
  let table = state.s_table in
  Hashtbl.iter table ~f:(fun ~key:name ~data:bindings ->
      if name <> "self" then (
        List.iter bindings ~f:(fun bind ->
            let ty = bind.bind_ty in
            match ty.ty with
            | Class_ty(name, _, Some(super)) -> (
                if not(Type.is_unkown_ty super) then (
                  let res = Printer.type_to_str super 0 in
                  Printf.printf "res: %s\n" res;
                  let super_name = Type.classty_get_name super in
                  ignore(Hashtbl.add class_hash ~key:name ~data:super_name);
                )
              )
            | _ -> ()
          )
      )
    );
  let res = ref "digraph G {
  ratio=\"fill\";
  fontsize=\"12pt\";
  rankdir = LR;\n" in
  let setted = Hashtbl.Poly.create() in
  Hashtbl.iter class_hash ~f:(fun ~key:base ~data:super ->
      if Hashtbl.mem setted base = false then (
        Hashtbl.add_exn setted ~key:base ~data:true;
        res := !res ^ Printf.sprintf "\"%s\" [style=filled, color=darkturquoise];\n" base);
      if Hashtbl.mem setted super = false then(
        Hashtbl.add_exn setted ~key:super ~data:true;
        res := !res ^ Printf.sprintf "\"%s\" [style=filled, color=darkturquoise];\n" super);
      res := !res ^ Printf.sprintf "\"%s\" -> \"%s\";\n" base super;
    );
  res := !res ^ "}\n";
  !res
