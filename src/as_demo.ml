open Sys
open Async.Std
open Core

let (r,w) = Pipe.create ()

(* producer *)
let countup hi w =
  let rec loop i =
    printf "i=%d\n" i ;
    if (i < hi &&( not (Pipe.is_closed w))) then
       Pipe.write w i >>>
       fun () -> loop (i+1)
     else Pipe.close w
  in
  loop 0 ;;

(* consumer *)
let rec readloop r =
  Pipe.read r >>=
  function
  | `Eof -> return (printf "finished\n"; exit 0)
  | `Ok v -> return (printf "Got %d\n" v) >>=
             fun () -> after (Time.Span.of_sec 0.1) >>=
             fun () -> readloop r  ;;


let () =
  Pipe.set_size_budget r 256  ;
  countup 100 w;
  ignore(readloop r);
  Core.Never_returns.never_returns (Scheduler.go ())

