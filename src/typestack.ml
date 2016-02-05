module TypeStack =
  struct
    type 'a ts = ('a * 'a) list
    type 'a elem = ('a * 'a)

    let empty = []

    let push (ts: 'a ts) (a: 'a) (b: 'a) : 'a ts =
      (a, b) :: ts

    let rec pop (ts: 'a ts) =
      match ts with
      | [] -> failwith "TypeStack pop empty"
      | [a] -> []
      | _ :: rest -> pop rest

    let size (ts: 'a ts) : int =
      List.length ts

    let rec contains (ts: 'a ts) (a: 'a) (b: 'a) : bool =
      match ts with
      | [] -> false
      | x :: rest -> (
          match x with
          | (_a, _b) -> true
          | _ -> contains rest a b
        )

  end
