open Unix

let bytes = 78

let path = Sys.argv.(1)

let cmd1 = "../itextile < " ^ path
let cmd2 = "../itextile_duce < " ^ path

let chan1 = open_process_in cmd1
let chan2 = open_process_in cmd2

let rec mainloop () =
  let line1 = input_line chan1 in
  let line2 = input_line chan2 in
  let len =
    if (String.length line1) >= (String.length line2)
    then String.length line1
    else String.length line2 in
  for i = 0 to len - 1 do
    if line1.[i] != line2.[i] then
      let pos = if i <= (bytes/2) then 0 else i - (bytes/2) in
      for j = pos to pos + bytes do
        print_char line1.[j];
      done;
      print_newline ();
      for j = pos to pos + bytes do
        print_char line2.[j];
      done;
      exit 1
  done;
  mainloop ()

let _ =
  try mainloop ()
  with End_of_file ->
    print_endline "End of file, done";
    exit 0

