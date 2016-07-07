open Core.Std
open Sys
open Unix

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
             match (Unix.stat f).st_kind with
             | S_REG -> (dirs, f::files)  (* Regular file *)
             | S_DIR -> (f::dirs, files)  (* Directory *)
             | _ -> (dirs, files)
          ) ~init:([], []) contents
      in
      let matched = List.filter ~f:(select) files in
      walk (matched @ acc) (dirs @ tail)
  in
  walk [] [dir]

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
  String.substr_index name "$" = Some(0)

let make_tag_id id tag =
  id ^ "^" ^ tag



