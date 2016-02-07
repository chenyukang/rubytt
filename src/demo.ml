open Core.Std


type record = {
  a : int;
  mutable b : int;
}

type demo = {
  c : int;
  mutable d: record;
}

let () =
  let x = { a = 1; b = 2} in
  let m = { c = 3; d = x } in
  let n = { c = 4; d = x } in 
  x.b <- 3;
  Printf.printf "m: %d\n" m.d.b;
  Printf.printf "n: %d\n" n.d.b;
  m.d.b <- 4;
  Printf.printf "prev: %d\n" x.b;
  Printf.printf "n: %d\n" n.d.b;
