
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


