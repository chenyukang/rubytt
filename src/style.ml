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

type style = {
  ty: style_ty;
  ss: int;
  ee: int;
  mutable msg: string;
  mutable url: string;
  mutable id: string;
  highlight: string list;
}

let new_style ty ss ee =
  {
    ty = ty; ss = ss; ee = ee;
    msg = ""; url = ""; id = "";
    highlight = [];
  }

type tag = {
  offset: int;
  sty: style;
}

type applier = {
  buffer: string;
  mutable tags: tag list;
  offset: int;
  file: string;
}

let apply file source styles =
  let applier = {
    buffer = "";
    tags = [];
    offset = 0;
    file = file;
  } in
  List.iter styles ~f:(fun s ->
      let start_tag = { offset = s.ss; sty = s} in
      let end_tag = { offset = s.ee; sty = s} in
      applier.tags <- applier.tags @ [start_tag; end_tag]
    );
  source
