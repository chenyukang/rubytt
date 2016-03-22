open Core.Std;;
open Type;;
open Style;;

let file_styles = Hashtbl.Poly.create();;
let root_path = ref "";;
let outdir = ref "";;
let methods_count = ref 0;;
let class_count = ref 0;;

let new_linker root out_dir =
  root_path := root;
  outdir := out_dir

let add_file_style path style =
  let styles = Hashtbl.find file_styles path in
  match styles with
  | Some(style_list) ->
    Hashtbl.replace file_styles ~key:path ~data:(style_list @ [style])
  | _ -> (Hashtbl.add_exn file_styles ~key:path ~data:[style])

let process_def bind =
  Printf.printf "process_def: %s\n%!" bind.qname;
  let qname = bind.qname in
  let style = Style.new_style ANCHOR bind.start bind.tail in
  style.msg <- Printer.type_to_str bind.bind_ty 0;
  style.url <- qname;
  style.id <- qname;
  add_file_style bind.bind_file style

let find_links bindings =
  Printf.printf "length: %d\n" (List.length bindings);
  List.iter bindings ~f:(fun bind ->
      (match bind.kind with
      | MethodK -> ( incr methods_count )
      | ClassK -> (incr class_count)
      | _ -> ());
      process_def bind;
    )




