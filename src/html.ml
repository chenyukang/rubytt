open Core.Std
open Sys

let markup file =
  let source = Util.read_file_to_str file in
  let css = Util.read_file_to_str "./show.css" in
  let js = Util.read_file_to_str "./highlight.js" in
  let result = "<html><head title=\"" ^ file ^ "\">" ^
               "<style type='text/css'>\n" ^ css ^ "</style>\n" ^
               "<script language=\"JavaScript\" type=\"text/javascript\">\n" ^ js ^
               "</script>\n" ^
               "</head>\n<body>\n" ^
               "<pre>" ^ source ^ "</pre>" ^
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
