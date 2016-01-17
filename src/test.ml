open Node
open Alcotest
open OUnit

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
  assert_equal (List.nth list 0) nil_node;
  add_children p list;
  assert_equal nil_node.parent (Some p);
  assert_equal (List.nth list 1).parent (Some p);
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

let test_unit = [
  "Node", `Quick, test_node;
  "Node_add_children", `Quick, test_node_add_children;
  "Node_block", `Quick, test_block;
]

let () =
  Alcotest.run "My Test" [ "test_unit", test_unit;]
