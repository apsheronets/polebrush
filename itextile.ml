
let text = Stream.from (fun _ -> try Some (read_line ())
  with End_of_file -> None)

let textile = Textile.of_stream text

let xhtml = Textile.to_xhtml textile

let _ =
  Stream.iter (print_string) xhtml;
  exit 0
