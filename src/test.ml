open Core.Std
open Alcotest
open Node
open Printer
open Util
open Parser
open Typestack
open Type

let run_dir dir =
  (* process rb to json *)
  Util.prepare_dump();
  Sys.command_exn (Printf.sprintf "ruby src/dump.rb %s %s" dir dir);

  let jsons = Util.walk_directory_tree dir ".*\\.json" in
  List.filter ~f:(fun j ->
                  Global.clear();
                  Checker.clear();
                  let base = Filename.chop_extension j in
                  let ast = build_ast_from_json j in
                  let _ = Trans.transform_expr ast Type.global_table in
                  let _ = Checker.traverse [ast] in
                  let ast_str = node_to_str ast 0 in
                  let tys_str = table_to_str Type.global_table 0 true in
                  let unused_str = Checker.env_unused true true in
                  let undef_str = Checker.env_undef() in
                  let sep_str = "\n\n" ^ (String.init 40 ~f:(fun _ -> '-')) ^ "\n\n" in
                  let log = Printf.sprintf "%s.log" base in
                  let cmp = Printf.sprintf "%s.cmp" base in
                  Out_channel.write_all
                    log ~data: (ast_str ^ sep_str ^ tys_str ^ sep_str ^
                                  "\nunused result:" ^ unused_str ^
                                  "\n\nundef result:" ^ undef_str);
                  Sys.command_exn (Printf.sprintf "rm %s" j);
                  not(cmp_file cmp log)
                 ) jsons

let run_check_dir dir =
  let filter_header str =
    let lines = Str.split (Str.regexp "\n") str in
    let lines = List.filter lines ~f:(fun l -> not (Util.contains l "Parsing and dump")) in
    List.fold lines ~init:"" ~f:(fun acc l -> if acc = "" then l else acc ^ "\n" ^ l) in
  let sub_dirs = Util.sub_dirs dir in
  List.iter sub_dirs ~f:(fun d -> Printf.printf "dir: %s\n" d);
  List.filter sub_dirs ~f:(fun d ->
                         let cmd = Printf.sprintf "./bin/main.byte -s %s -t check" d in
                         let result = filter_header (Util.read_process cmd) in
                         let cmp = Printf.sprintf "%s.cmp" d in
                         let log = Printf.sprintf "%s.log" d in
                         Out_channel.write_all log ~data:result;
                         ignore(Printf.printf "result:%s\n" result);
                         not(cmp_file cmp log))
  
let update_cmp dir =
  let logs = Util.walk_directory_tree dir ".*\\.log" in
  List.iter ~f:(fun p ->
      let b = Filename.chop_extension p in
      let o = Printf.sprintf "%s.cmp" b in
      Sys.command_exn (Printf.sprintf "cp %s %s" p o);
    ) logs


let test_dir() =
  let res = run_dir "tests/cases" in
  if (List.length res <> 0) then Printf.printf "\n";
  List.iter res ~f:(fun p -> Printf.printf "fail case: %s\n" p);
  if List.length res <> 0 then failwith "testing failed"

let test_checker() =
  let res = run_check_dir "tests/checker" in
  if (List.length res <> 0) then Printf.printf "\n";
  List.iter res ~f:(fun p -> Printf.printf "fail checker case: %s\n" p);
  if List.length res <> 0 then failwith "checker testing failed"
                                        
let test_unit = [
    "Cases", `Quick, test_dir;
    "Checker", `Quick, test_checker;
]

let () =
  if Array.length Sys.argv = 2 then
    let arg = Array.nget Sys.argv 1 in
    if arg = "-update" || arg = "-u" then
      update_cmp "./tests"
  else
    Alcotest.run "My Test" [ "test_unit", test_unit;]
