open Core.Std
open Sys
open Style

let css = "
    body { color: #666666; border: 1px solid #dddddd; padding: 1in; }
    a {
      text-decoration: none; color: #5AA2A7;
      border: solid 1px white;
    }
    a.active {
      background: -webkit-linear-gradient(top,rgba(255, 255, 200, 0.35) 0,rgba(255, 255, 200, 0.55) 100%);
      border: solid 1px #E5E600;
    }
    table, th, td { border: 1px solid lightgrey; padding: 5px; corner: rounded; }
    .builtin {color: #B17E41;}
    .comment, .block-comment {color: #aaaaaa; font-style: italic;}
    .constant {color: #888888;}
    .decorator {color: #778899;}
    .doc-string {color: #aaaaaa;}
    .error {border-bottom: 1px solid red;}
    .field-name {color: #2e8b57;}
    .function {color: #4682b4;}
    .identifier {color: #8b7765;}
    .info {border-bottom: 1px dotted RoyalBlue;}
    .keyword {color: #0000cd;}
    .lineno {color: #dddddd;}
    .number {color: #483d8b;}
    .parameter {color: #777777;}
    .string {color: #999999;}
    .type-name {color: #4682b4;}
    .warning {border-bottom: 1px dotted orange;}"

let js = "
    var highlighted;
    function highlight(xid) {
        var elms = document.querySelectorAll('[xid=\"' + xid + '\"]');
        for (k in elms) {
            v = elms[k]
            v.className=\"active\";
        }
        highlighted = xid;
    }

    function clearHighlight() {
        var elms = document.querySelectorAll('[xid=\"' + highlighted + '\"]');
        for (k in elms) {
            v = elms[k]
            v.className=\"\";
        }
    }

    window.onload =
        function (e) {
            var tags = document.getElementsByTagName(\"A\")
            for (var i = 0; i < tags.length; i++) {
                tags[i].onmouseover =
                    function (e) {
                        clearHighlight();
                        var xid = e.toElement.getAttribute('xid');
                        highlight(xid);
                    }
            }
   }
"

let add_line source =
  let lines = Str.split (Str.regexp "\n") source in
  let result = ref "" in
  List.iteri lines ~f:(fun i l ->
      result := !result ^ (
          "<span class='lineno'>"
          ^ (Printf.sprintf "%4d  " (i+1))
            ^ "</span>" ^ l ^ "\n"
        ));
  !result

let markup file =
  let styles = Linker.get_styles_for_file file in
  let source = Util.read_file_to_str file in
  let source_with_style = Style.apply file source styles in
  let result = "<html><head title=\"" ^ file ^ "\">" ^
               "<style type='text/css'>\n" ^ css ^ "</style>\n" ^
               "<script language=\"JavaScript\" type=\"text/javascript\">\n" ^ js ^
               "</script>\n" ^
               "</head>\n<body>\n" ^
               "<pre>" ^ (add_line source_with_style) ^ "</pre>" ^
               "</body></html>" in
  result

let dump_html input output_dir =
  let root_dir = match Sys.is_directory_exn input with
    | true -> input
    | _ -> Sys.getcwd() in
  Linker.new_linker root_dir output_dir;
  Linker.find_links !Global.bindings;
  Linker.linker_print();
  Printf.printf "Writing HTML\n%!";
  Hash_set.iter Global.loaded_files ~f:(
    fun f ->
      Printf.printf "generate file: %s\n%!" f;
      let out = Util.change_root_dir_of_path root_dir output_dir f in
      let outfile = Util.change_extension out ".html" in
      let html = markup f in
      Out_channel.write_all outfile ~data: html;
  )
