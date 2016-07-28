open Core.Std
open Sys
open Style

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
  let css = Util.read_file_to_str "./show.css" in
  let js = Util.read_file_to_str "./highlight.js" in
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
