open Core.Std

type style_ty =
  | KEYWORD
  | COMMENT
  | STRING
  | DOC_STRING
  | IDENTIFIER
  | BUILTIN
  | NUMBER
  | CONSTANT
  | FUNCTION
  | PARAMETER
  | LOCAL
  | DECORATOR
  | CLASS
  | ATTRIBUTE
  | LINK
  | ANCHOR
  | DELIMITER
  | TYPE_NAME
  | ERROR
  | WARNING
  | INFO

type tag_ty =
  | START
  | END

type style = {
  ty: style_ty;
  ss: int;
  ee: int;
  mutable msg: string;
  mutable url: string;
  mutable id: string;
  highlight: string list;
}

let str_of_style sty =
  Printf.sprintf "[%s start=%d end=%d]" "link" sty.ss sty.ee

let new_style ty ss ee =
  {
    ty = ty; ss = ss; ee = ee;
    msg = ""; url = ""; id = "";
    highlight = [];
  }

type tag = {
  offset: int;
  sty: style;
  tag_ty: tag_ty;
}

type applier = {
  mutable buffer: string;
  mutable tags: tag list;
  mutable cur: int;
  file: string;
}

let to_css sty =
  let str = str_of_style sty in
  Stringext.replace_all str ~pattern:"_" ~with_:"-"

let escape str =
  let a = ["&"; "'"; "\""; "<";  ">"] in
  let b = ["&amp;"; "&#39;"; "&quot;"; "&lt;"; "&gt"] in
  let res = ref str in
  List.iteri a ~f:(fun i x ->
      let r = List.nth_exn b i in
      res := Stringext.replace_all !res ~pattern:x ~with_:r
    );
  !res

let apply_tag applier source (t:tag) =
  let add buf =
    applier.buffer <- applier.buffer ^ buf in
  if t.offset > applier.cur then (
    let append = String.sub source ~pos:applier.cur ~len:(t.offset - applier.cur) in
    let escp = escape append in
    applier.cur <- t.offset;
    add escp
  );
  match t.tag_ty with
  | START -> (
      (match t.sty.ty with
      | ANCHOR -> (add ("<a name='" ^ t.sty.url ^ "'");
                   add (", xid='" ^ t.sty.id ^ "'"))
      | LINK -> (add ("<a href='" ^ t.sty.url ^ "'");
                 add (", xid='" ^ t.sty.id ^ "'"))
      | _ -> add ("<span class='" ^ to_css(t.sty) ^ "'"));
      if t.sty.msg <> "" then
        add (Printf.sprintf ", title='%s'" t.sty.msg);
      add ">"
    )
  | _ -> (
      match t.sty.ty with
      | ANCHOR | LINK -> add "</a>"
      | _ -> add "</span>"
    )

let apply file source styles =
  let applier = { buffer = ""; tags = []; cur = 0; file = file } in
  List.iter styles ~f:(fun s ->
      let start_tag = { offset = s.ss; sty = s; tag_ty = START} in
      let end_tag = { offset = s.ee; sty = s; tag_ty = END } in
      applier.tags <- applier.tags @ [start_tag; end_tag]
    );
  applier.tags <- List.sort applier.tags ~cmp:(fun t1 t2 -> t1.offset - t2.offset);
  List.iter applier.tags ~f:(fun tag -> apply_tag applier source tag);
  let len = String.length source in
  if applier.cur < len then (
    let append = String.sub source ~pos:applier.cur ~len:(len - applier.cur) in
    let escp = escape append in
    applier.cur <- len;
    applier.buffer <- applier.buffer ^ escp
  );
  applier.buffer
