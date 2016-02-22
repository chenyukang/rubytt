open Core.Std
open Sys

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

let do_command command =
  Sys.command command

let cmp_file a b =
  if (Sys.file_exists_exn a = false || Sys.file_exists_exn b = false) then
    false
  else
    let _a = read_file_to_str a in
    let _b = read_file_to_str b in
    if _a <> _b then false else true

let extension f =
  match Filename.split_extension f with
  |(_, Some(b)) -> b
  | _ -> ""


let run_dump_ruby filename =
  let b = Filename.chop_extension filename in
  let j = Printf.sprintf "%s.json" b in
  Sys.command_exn (Printf.sprintf "ruby dump_ruby.rb %s %s /tmp/res" filename j);
  j

let update_cmp dir =
  let files = Array.to_list (Sys.readdir dir) in
  let logs = List.filter files ~f:(fun x -> extension x = "log") in
  List.map ~f:(fun f ->
      let p = Filename.concat dir f in
      let b = Filename.chop_extension p in
      let o = Printf.sprintf "%s.cmp" b in
      Sys.command_exn (Printf.sprintf "cp %s %s" p o);
    ) logs

let main_name tagged_name =
  let segs = Str.split (Str.regexp "\\^") tagged_name in
  if List.length segs > 0 then
    List.nth_exn segs 0
  else
    tagged_name


let is_synthetic_name name =
  name = "self" || name = "#this"

let is_global_name name =
  String.substr_index name "$" = Some(0)

