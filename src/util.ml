open Core.Std
open Sys
open Unix

let prepare_dump () =
  Out_channel.write_all "/tmp/dump.rb" ~data: Dump.dump_str

let read_file_to_str name =
  In_channel.read_all name

let read_process command =
  let buffer_size = 2048 in
  let buffer = Buffer.create buffer_size in
  let string = String.create buffer_size in
  let in_channel = Unix.open_process_in command in
  let chars_read = ref 1 in
  while !chars_read <> 0 do
    chars_read := input in_channel string 0 buffer_size;
    Buffer.add_substring buffer string 0 !chars_read
  done;
  ignore (Unix.close_process_in in_channel);
  Buffer.contents buffer

let cmp_file a b =
  if (Sys.file_exists_exn a = false || Sys.file_exists_exn b = false) then
    false
  else
    (read_file_to_str a) = (read_file_to_str b)

                             
let walk_directory_tree dir pattern =
  let select str = Str.string_match (Str.regexp pattern) str 0 in
  let rec walk acc = function
  | [] -> (acc)
  | dir::tail ->
      let contents = Array.to_list (Sys.readdir dir) in
      let contents = List.rev_map ~f:(Filename.concat dir) contents in
      let dirs, files =
        List.fold_left ~f:(fun (dirs, files) f ->
                           try
                             match (Unix.stat f).st_kind with
                             | S_REG -> (dirs, f::files)  (* Regular file *)
                             | S_DIR -> (f::dirs, files)  (* Directory *)
                             | _ -> (dirs, files)
                           with
                             _ -> (dirs, files)
                          ) ~init:([], []) contents
      in
      let matched = List.filter ~f:(select) files in
      walk (matched @ acc) (dirs @ tail)
  in
  walk [] [dir]

let sub_dirs dir =
  let contents = Array.to_list (Sys.readdir dir) in
  let contents = List.rev_map ~f:(Filename.concat dir) contents in
  List.filter contents ~f:(fun s ->
                           try match (Unix.stat s).st_kind with
                               | S_DIR -> true | _ -> false
                           with _ -> false)
              
let main_name tagged_name =
  let segs = Str.split (Str.regexp "\\^") tagged_name in
  if List.length segs > 0 then
    List.nth_exn segs 0
  else tagged_name


let change_root_dir_of_path input_dir output_dir file =
  let abs_in = Filename.realpath input_dir in
  let abs_ou = Filename.realpath output_dir in
  let abs_file = Filename.realpath file in
  abs_ou ^ (String.drop_prefix abs_file (String.length abs_in))

let change_extension file ext =
  let base = Filename.chop_extension file in
  base ^ ext

let is_synthetic_name name =
  name = "self" || name = "#this"

let is_global_name name =
  String.substr_index name ~pattern:"$" = Some(0)

let make_tag_id id tag =
  id ^ "^" ^ tag

let contains search target =
  String.substr_index search ~pattern:target <> None

let string_to_int str =
  if String.length str = 0 then 0
  else (
    match String.get str 0 with
    | '-' -> -(Int.of_string (String.substr_replace_first str ~pattern:"-" ~with_:""))
    | '+' -> (Int.of_string (String.substr_replace_first str ~pattern:"+" ~with_:""))
    | _ -> Int.of_string str
  )

let string_to_float str =
  if String.length str = 0 then 0.0
  else (
    match String.get str 0 with
    | '-' -> -.(Float.of_string (String.substr_replace_first str ~pattern:"-" ~with_:""))
    | '+' -> (Float.of_string (String.substr_replace_first str ~pattern:"+" ~with_:""))
    | _ -> Float.of_string str
  )

(* rails pluralize class name to table_name *)
let rules =
  List.map ~f:(fun x -> (Str.regexp (fst x)),(snd x))
    ["\\([psc]h\\)$\\|z$","\\0es";
     "\\(ff\\)$\\|\\(ey\\)$","\\0s";
     "f$","ves";
     "y$","ies";
     "ix$","ices";
     "ius$","ii";
     "[sx]$","\\0es";
     "non","na"];;


let pluralize x = (* "wish" in *)
  let f w x =
  ignore(Str.search_forward (fst x) w 0);
  Str.replace_first (fst x) (snd x) w in
  let rec exn_map ex fn1 fn2 l =
    match l with
      [] -> fn2
    | h::t -> try (fn1 h) with _ex -> exn_map ex fn1 fn2 t in
  exn_map Not_found (f x) (x ^ "s") rules;;

let uncapitalize_class_name class_name =
  let res = String.foldi class_name ~init:"" ~f:(fun i acc c ->
      if i <> 0 && Char.is_uppercase c then
        acc ^ (Printf.sprintf "_%c" (Char.lowercase c))
      else
        acc ^ (Char.to_string (Char.lowercase c))
    ) in
  (String.substr_replace_first res ~pattern:"::" ~with_:"")

let class_to_table_name class_name =
  pluralize (uncapitalize_class_name class_name)

let remove_elt e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []

let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l []
