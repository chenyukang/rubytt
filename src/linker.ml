open Core.Std;;
open Type;;

let file_styles = Hashtbl.Poly.create();;
let root_path = ref "";;
let outdir = ref "";;
let methods_count = ref 0;;
let class_count = ref 0;;

let new_linker root out_dir =
  root_path := root;
  outdir := out_dir


let find_links bindings =
  List.iter bindings ~f:(fun bind -> ())


