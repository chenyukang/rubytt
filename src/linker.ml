open Core.Std;;
open Type;;
open Node;;
open Style;;

let file_styles = Hashtbl.Poly.create();;
let root_path = ref "";;
let outdir = ref "";;
let methods_count = ref 0;;
let class_count = ref 0;;
let seen_def = Hash_set.Poly.create();;
(* let seen_ref = Hash_set.Poly.create();; *)

let new_linker root out_dir =
  root_path := root;
  outdir := out_dir

let add_file_style path style =
  let styles = Hashtbl.find file_styles path in
  match styles with
  | Some(style_list) ->
    Hashtbl.replace file_styles ~key:path ~data:(style_list @ [style])
  | _ -> (Hashtbl.add_exn file_styles ~key:path ~data:[style])

let get_styles_for_file path =
  let path = Filename.realpath path in
  match Hashtbl.find file_styles path with
  | Some(styles) -> styles
  | _ -> []

let process_def bind =
  (* Printf.printf "process_def: %s\n bind_file: %s ss:%d ee:%d\n%!" *)
  (*   bind.qname bind.bind_file bind.start bind.tail; *)
  let hash_str = Type.bind_hash_str bind in
  if Hash_set.mem seen_def hash_str = false then (
    Hash_set.add seen_def hash_str;
    let qname = bind.qname in
    let style = Style.new_style Style.ANCHOR bind.start bind.tail in
    (* style.msg <- Printer.type_to_str bind.bind_ty 0; *)
    style.url <- qname;
    style.id <- qname;
    add_file_style bind.bind_file style
  )

let process_ref ref_node bindings =
  let info = ref_node.info in
  let hash = Node.node_t_hash ref_node in
  let style = Style.new_style Style.LINK info.ss info.ee in
  let msg =
    List.fold_left bindings
      ~init:""
      ~f:(fun acc bind -> acc ^ "|" ^ (Printer.type_to_str ~show_bind:false bind.bind_ty 0)) in
  style.msg <- msg;
  (* FIXME *)
  (match List.find bindings ~f:(fun bind -> bind.qname <> "") with
   | Some(b) -> style.url <- b.qname; style.id <- b.qname
   | _ -> ());
  (* Printf.printf "process_ref: bind_name: %s ss:%d ee:%d\n%!" *)
  (*   style.id info.ss info.ee; *)
  add_file_style info.file style

let find_links bindings =
  Printf.printf "length: %d\n" (List.length bindings);
  List.iter bindings ~f:(fun bind ->
      (match bind.kind with
      | MethodK -> ( incr methods_count )
      | ClassK -> (class_count := !class_count + 1)
      | _ -> ());
      process_def bind;
    );
  Printf.printf "Global size: %d\n" (Hashtbl.length Global.refs);
  Hashtbl.iter Global.refs
    ~f:(fun ~key:node ~data:bindings -> process_ref node bindings)

let linker_print () =
  Printf.printf "methods: %d\n" !methods_count;
  Printf.printf "class:   %d\n" !class_count
