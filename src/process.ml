(* let p () = *)
(*   for i = 0 to 5 do *)
(*     let oc_in, oc_out = Unix.open_process "irb" in *)
(*     match Unix.fork() with *)
(*     | 0 -> ( *)
(*         ignore(Unix.close_process_out oc_out); *)
(*       ) *)
(*     | _ -> ( *)
(*         let m = Printf.sprintf "load \"dump_ruby.rb\"; my_file = File.new(\"simple_file%d.txt\", \"w+\"); my_file.close;" i in *)
(*         output_string oc_out m; *)
(*         flush oc_out; *)
(*       ) *)
(*   done;; *)


(* p(); *)
(* let p () = *)
(*   let oc_out = Unix.open_process_out "irb" in *)

(*   for i = 0 to 5 do *)
(*     match Unix.fork() with *)
(*     | 0 -> ( *)
(*         ignore(Unix.close_process_out oc_out); *)
(*       ) *)
(*     | _ -> ( *)
(*         let m = Printf.sprintf "load \"dump_ruby.rb\"; my_file = File.new(\"simple_file%d.txt\", \"w+\"); my_file.close;" i in *)
(*         output_string oc_out m; *)
(*         flush oc_out; *)
(*       ) *)
(*   done;; *)

(* p(); *)

(*  open Sys;; *)
(*  open Unix;; *)

(* let (fd_in, fd_out) = pipe () in *)
(* match fork () with *)
(* | 0 -> *)
(*   (\* dup2 stdin fd_in; *\) *)
(*   (\* close stdin; *\) *)
(*   execv "irb" [| "irb"|] *)
(* | _ -> *)
(*   let m = "load \"dump_ruby.rb\"; my_file = File.new(\"simple_file.txt\", \"w+\"); my_file.close;" in *)
(*   ignore(Unix.write fd_in m 0 (String.length m)); *)
(*   let rec wait_for_children retcode = *)
(*     try *)
(*       match wait () with *)
(*       | (pid, WEXITED n) -> wait_for_children (retcode lor n) *)
(*       | (pid, _)         -> wait_for_children 127 *)
(*     with *)
(*       Unix_error(ECHILD, _, _) -> retcode in *)
(*   exit (wait_for_children 0);; *)



let output, input = Unix.pipe() in
let _ = Unix.create_process "irb" [|"irb"|] input Unix.stdout Unix.stderr in
begin
  (try
     Printf.printf "here\n";
     (* let m = "load \"dump_ruby.rb\"; my_file = File.new(\"simple_file.txt\", \"w+\"); my_file.close;" in *)
     let m = "sleep 10;" in
    ignore(Unix.write input m 0 (String.length m));
    Printf.printf "now\n";
    Unix.close input;
  with
    Unix.Unix_error(n, f, arg) ->
    Printf.printf "%s(%s) : %s\n" f arg (Unix.error_message n))
end
  let s = ref ""  and  buff = Bytes.create 500 in
  while true do
    match Unix.read output buff 0 500 with
      0 -> Printf.printf "My grandchildren are %s\n" !s ; exit 0
    | n -> s := !s ^ (String.sub buff 0 n) ^ "."
  done





(* let output, input = Unix.pipe();; *)

(* let write_pid input = *)
(*   try *)
(*     let m = "(" ^ (string_of_int (Unix.getpid ())) ^ ")" *)
(*     in ignore (Unix.write input m 0 (String.length m)) ; *)
(*        Unix.close input *)
(*    with *)
(*      Unix.Unix_error(n,f,arg) -> *)
(*        Printf.printf "%s(%s) : %s\n" f arg (Unix.error_message n) ;; *)

(* match Unix.fork () with *)
(*   0 -> ( *)
(*     let _ = Unix.create_process "irb" [|"irb"|] Unix.stdin Unix.stdout Unix.stderr in ( *)
(*       match Unix.fork() with *)
(*         0 -> ( *)
(*           let m = "my_file = File.new(\"simple_file.txt\", \"w+\"); my_file.close;"  *)
(*         in ignore (Unix.write Unix.stdin m 0 (String.length m)) *)
(*         ) *)
(*       | _ -> *)
(*         let m = "my_file = File.new(\"simple_file.txt\", \"w+\"); my_file.close;"  *)
(*         in ignore (Unix.write Unix.stdin m 0 (String.length m))); *)
(*   ) *)
(* | _  -> Unix.close input; *)
(*   let s = ref ""  and  buff = Bytes.create 500 *)
(*   in while true do *)
(*     match Unix.read output buff 0 500 with *)
(*       0 -> Printf.printf "My grandchildren are %s\n" !s ; exit 0 *)
(*     | n -> s := !s ^ (String.sub buff 0 n) ^ "." *)
(*   done ;; *)
