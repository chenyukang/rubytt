open Core.Std
open Alcotest
open OUnit
open Node
open Printer
open Util
open Parser
open Typestack
open Type
open State
open Binding

let run_dir dir =
  let files = Array.to_list (Sys.readdir dir) in
  let rb = List.filter files ~f:(fun x -> extension x = "rb") in
  List.map ~f:(fun f ->
      Printf.printf "\nnow: %s\n" f;
      let p = Filename.concat dir f in
      let b = Filename.chop_extension p in
      begin
        let j = run_dump_ruby p in
        let ast = parse_file j in
        let ast_str = node_to_str ast 0 in
        let log = Printf.sprintf "%s.log" b in
        let cmp = Printf.sprintf "%s.cmp" b in (
          Out_channel.write_all log ~data: ast_str;
          (* Sys.command_exn (Printf.sprintf "rm %s" j); *)
          if cmp_file cmp log then (
            (* Printf.printf "pass: %s\n" p; *)
            true)
          else (
            Printf.printf "fail: %s\n\n" p;
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

let test_typestack() =
  let a = TypeStack.empty in
  let a = TypeStack.push a 1 2 in
  assert_equal (TypeStack.size a) 1;
  let a = TypeStack.pop a in
  assert_equal (TypeStack.size a) 0

let test_state() =
  let a = State.new_state ~parent:None State.Class in
  assert_equal a.parent None

let test_bool_type() =
  let b = Type.new_bool_type Undecided None None in(
  match b.ty with
  | Bool_ty(v, s1, s2) ->
    if not (v = Undecided && s1 = None && s2 = None) then
      assert_failure "default bool value"
  | _ -> assert_failure "default bool type error"
  );
  let s = State.new_state ~parent:None State.Class in
  bool_set_s2 b (Some s);
  (match b.ty with
  | Bool_ty(v, s1, s2) ->
    if not(v = Undecided && s2 <> None) then assert_failure "bool_set_s1 failed"
  | _ -> ()
  );
  let b = bool_swap b in (
  match b.ty with
  | Bool_ty(_, s1, s2) ->
    if not(s1 <> None && s2 = None) then assert_failure "bool_swap failed"
  | _ -> ())

let test_type() =
  let a = new_bool_type Undecided None None in
  let b = new_bool_type Undecided None None in
  assert_equal (is_num_type a || is_str_type a) false;
  assert_equal (type_equal a b) true;
  assert_equal (is_mutated a) false;
  set_file a "file";
  assert_equal (a.info.file) "file";
  set_mutated a true;
  assert_equal (is_mutated a) true;
  assert_equal (is_undecided_bool a) true

let test_union_type() =
  ()

let test_dir() =
  let res = run_dir "tests" in
  assert_equal (List.exists res ~f:(fun x -> x = false)) false

let test_unit = [
  "Node", `Quick, test_node;
  "Node_add_children", `Quick, test_node_add_children;
  "Node_block", `Quick, test_block;
  "Printer", `Quick, test_printer;
  "TypeStack", `Quick, test_typestack;
  "State", `Quick, test_state;
  "Bool", `Quick, test_bool_type;
  "Type", `Quick, test_type;
  "Union", `Quick, test_union_type;
  "Cases", `Quick, test_dir;
]

let () =
  Alcotest.run "My Test" [ "test_unit", test_unit;]
