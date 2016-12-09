open Core.Std
open Sys
open Unix

let () =
  let dump_str = In_channel.read_all "dump.rb" in
  let escaped = String.escaped dump_str in
  Out_channel.write_all "dump.ml" ~data:("let dump_str = \"" ^ escaped ^ "\"\n")
