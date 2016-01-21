open Core.Std
open Alcotest
open OUnit
open Node
open Printer
open Util
open Parser

let run_file dir =
  let files = Array.to_list (Sys.readdir dir) in
  List.iter files ~f:(fun f ->
      let p = Filename.concat dir f in
      Sys.command_exn (Printf.sprintf "ruby dump_ruby.rb %s %s.json /tmp/res" p p ))

let run_dir dir =
  let files = Array.to_list (Sys.readdir dir) in
  let rb = List.filter files ~f:(fun x -> extension x = "rb") in
  List.map ~f:(fun f ->
      Printf.printf "\nnow: %s\n" f;
      let p = Filename.concat dir f in
      let b = Filename.chop_extension p in
      let o = Printf.sprintf "%s.json" b in
      begin
        Sys.command_exn (Printf.sprintf "ruby dump_ruby.rb %s %s /tmp/res" p o );
        let ast = parse_file o in
        let ast_str = node_to_str ast 0 in
        let log = Printf.sprintf "%s.log" b in
        let cmp = Printf.sprintf "%s.cmp" b in (
          Out_channel.write_all log ~data: ast_str;
          Sys.command_exn (Printf.sprintf "rm %s" o);
          if cmp_file cmp log then (
            Printf.printf "pass: %s" p;
            true)
          else (
            Printf.printf "fail: %s" p;
            false)
        )
      end) rb

let test_node() =
  let nil = nil_node in
  let b = nil_node in
  assert_equal nil.ty Nil;
  assert_equal nil.parent None;
  set_node_path nil "path";
  assert_equal nil.info.path "path";
  assert_equal b.info.path "path"

(* contructo c -> b -> p *)
let test_node_add_children() =
  let a = nil_node in
  let b = make_nil_node "b" 1 2 in
  let c = make_nil_node "c" 1 2 in
  let list = [a; b] in
  let p = make_nil_node "file" 1 1 in
  assert_equal (List.length list) 2;
  assert_equal (List.nth_exn list 0) nil_node;
  add_children p list;
  assert_equal nil_node.parent (Some p);
  assert_equal (List.nth_exn list 1).parent (Some p);
  assert_equal b.parent (Some p );
  assert_equal (get_ast_root b) p;
  set_node_parent c b;
  assert_equal (get_ast_root c) p

(* test block *)
let test_block() =
  let a = make_nil_node "a" 1 2 in
  let b = make_nil_node "b" 1 2 in
  let c = make_nil_node "c" 1 2 in
  let list = [a; b; c] in
  let bl = make_block_node list "f" 1 2 in
  assert_equal (get_ast_root a).info.file "f";
  assert_equal (get_ast_root b).info.file "f";
  assert_equal (get_ast_root c).info.file "f";
  match bl.ty with
  |Block(stmts) -> assert_equal (List.length stmts) 3
  | _ -> assert_failure "impossible"

let test_printer() =
  let a = make_nil_node "f" 1 2 in
  let s = node_to_str a 0 in
  assert_equal s "(Nil)";

  let i = make_int_node "2" "f" 1 2 in
  let _i = node_to_str i 0 in
  assert_equal _i "(Int 2)"

let test_dir() =
  let res = run_dir "tests" in
  assert_equal (List.exists res ~f:(fun x -> x = false)) false

let test_unit = [
  "Node", `Quick, test_node;
  "Node_add_children", `Quick, test_node_add_children;
  "Node_block", `Quick, test_block;
  "Printer", `Quick, test_printer;
  "Cases", `Quick, test_dir;
]

let () =
  Alcotest.run "My Test" [ "test_unit", test_unit;]
