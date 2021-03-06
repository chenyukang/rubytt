open Core
open Type
open Node
open Style
open Def

let file_styles = Hashtbl.Poly.create()
let root_path = ref ""
let outdir = ref ""
let methods_count = ref 0
let class_count = ref 0
let seen_def = Hash_set.Poly.create()
(* let seen_ref = Hash_set.Poly.create();; *)

let new_linker root out_dir =
  root_path := root;
  outdir := out_dir

let to_url cur_file file name =
  if cur_file = file then (
    "#" ^ name
  ) else (
    let out = Util.change_root_dir_of_path !root_path !outdir file in
    "file:///" ^ (Util.change_extension out ".html" ^ "#" ^ name)
  )

let add_file_style path style =
  let styles = Hashtbl.find file_styles path in
  match styles with
  | Some(style_list) ->
    Hashtbl.set file_styles ~key:path ~data:(style_list @ [style])
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
    style.msg <- Printer.type_to_str ~show_bind:false bind.bind_ty 0;
    style.url <- qname;
    style.id <- qname;
    add_file_style bind.bind_file style
  )

let process_ref ref_node bindings =
  let info = ref_node.info in
  let hash = Node.node_t_hash ref_node in
  let style = Style.new_style Style.LINK info.ss info.ee in
  let strs = List.map bindings ~f:(fun b -> (Printer.type_to_str ~show_bind:false b.bind_ty 0)) in
  let msg =
    List.foldi (Util.remove_duplicates strs)
               ~init:""
               ~f:(fun i acc str ->
                   if i = 0 then str else acc ^ "|" ^ str) in
  style.msg <- msg;
  (match List.find bindings ~f:(fun bind -> bind.qname <> "" && bind.bind_file <> "") with
   | Some(b) -> (
       style.url <- (to_url info.file b.bind_file b.qname); style.id <- b.qname
     )
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
  Printf.printf "Global size: %d\n" (NodeHashtbl.length Global.refs);
  NodeHashtbl.iter (fun node bindings -> process_ref node bindings) Global.refs

let linker_print () =
  Printf.printf "methods: %d\n" !methods_count;
  Printf.printf "class:   %d\n" !class_count
