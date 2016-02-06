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
  x.b <- 3;
  Printf.printf "now: %d\n" m.d.b;
  m.d.b <- 4;
  Printf.printf "here: %d\n" x.b;
