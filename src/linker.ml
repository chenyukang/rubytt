open Core.Std;;
open Type;;
open Style;;

let file_styles = Hashtbl.Poly.create();;
let root_path = ref "";;
let outdir = ref "";;
let methods_count = ref 0;;
let class_count = ref 0;;
let seen_def = Hash_set.Poly.create();;

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
  (* Printf.printf "process_def: %s\n bind_file: %s\n%!" bind.qname bind.bind_file; *)
  let hash_str = Type.bind_hash_str bind in
  if Hash_set.mem seen_def hash_str = false then (
    Hash_set.add seen_def hash_str;
    let qname = bind.qname in
    let style = Style.new_style ANCHOR bind.start bind.tail in
    style.msg <- Printer.type_to_str bind.bind_ty 0;
    style.url <- qname;
    style.id <- qname;
    add_file_style bind.bind_file style
  )

let find_links bindings =
  Printf.printf "length: %d\n" (List.length bindings);
  List.iter bindings ~f:(fun bind ->
      (match bind.kind with
      | MethodK -> ( incr methods_count )
      | ClassK -> (class_count := !class_count + 1)
      | _ -> ());
      process_def bind;
    )

let linker_print () =
  Printf.printf "methods: %d\n" !methods_count;
  Printf.printf "class:   %d\n" !class_count
