(* make itextile && ./itextile < ~/wikies/komar.gikia.org/ru/tech/slax > ~/html.html && echo done *)
(* make itextile_duce && ./itextile_duce < ~/wikies/komar.gikia.org/ru/tech/slax > ~/ducehtml.html && echo done *)

let path1 = Sys.argv.(1)
let path2 = Sys.argv.(2)

let chan1 = open_in path1
let chan2 = open_in path2

let times f n =
  for i = 1 to n do
    f ()
  done

let rec loop () =
  try
    let c1 = input_char chan1 in
    let c2 = input_char chan2 in
    if c1 != c2
    then begin
      print_char c1;
      times (fun () -> print_char (input_char chan1)) 40;
      print_newline ();
      print_char c1;
      times (fun () -> print_char (input_char chan2)) 40;
      exit 1
    end else loop ()
  with End_of_file ->
    print_endline "End of file, done";
    exit 0

let _ = loop ()
