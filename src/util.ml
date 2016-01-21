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
