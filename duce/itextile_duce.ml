
let text = Stream.from (fun _ -> try Some (read_line ())
  with End_of_file -> None)

let textile = Textile.of_stream text

let xhtml = Stream.from (fun _ ->
  try Some (Textile_duce.xhtml_of_block (Stream.next textile))
  with Stream.Failure -> None)

let print_xhtml =
  Xhtmlpretty_duce.pretty_print_xhtml
    print_string

let _ =
  Stream.iter (print_xhtml) xhtml;
  exit 0
