open ExtLib

(* my loveley *)
let (@@) = List.rev_append
let (>>) f g = g f


(* * * * * * * * * * *
 * Type  declaration *
 * * * * * * * * * * *)

type phrase =
  | CData of string list
  | Emphasis of phrase    (* _ *)
  | Strong of phrase      (* * *)
  | Italic of phrase      (* __ *)
  | Bold of phrase        (* ** *)
  | Citation of phrase    (* ?? *)
  | Deleted of phrase     (* - *)
  | Inserted of phrase    (* + *)
  | Superscript of phrase (* ^ *)
  | Subscript of phrase   (* ~ *)
  | Span of phrase        (* % *)
  | Code of phrase        (* @ *)

  | Link of string * phrase
type alignment =
  | Right   (* > *)
  | Left    (* < *)
  | Center  (* = *)
  | Justify (* <> *)
type element =
  | Phrase of phrase
  | Element of element
type block =
  | Header1 of phrase        (* h1. *)
  | Header2 of phrase        (* h2. *)
  | Header3 of phrase        (* h3. *)
  | Blockquote of phrase     (* bq. *)
  | Footnote of phrase       (* fnn. *) (* FIXME *)
  | Paragraph of phrase      (* p.  *)
  | Blockcode of phrase      (* bc. *)
  | Pre of phrase            (* pre. *)
  | Numlist of element list  (* # *)
  | Bulllist of element list (* * *)
  (*| Table of FIXME *)

(*let read_file file =
  let chan = open_in file in
  let rec loop acc =
    try
      let result = input_line chan in
      loop (result::acc)
    with End_of_file -> acc
  in loop []*)

let read_file file =
  let channel = open_in file in
  let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
        try Some (input_line channel) with End_of_file -> None) in
  line_stream_of_channel channel

let s = read_file "test.txt"

let parse lines =
  (*let parse_block lines =
    Stream.of_string (List.hd lines)
  *)
  let rec next_block block_lines i =
    match Stream.peek lines, block_lines with
      | None, [] ->
          None
      | Some "", [] ->
          Stream.junk lines;
          next_block block_lines i
      | Some "", _ | None, _ ->
          Some (Paragraph (CData (List.rev block_lines)))
      | Some line, _ ->
          Stream.junk lines;
          next_block (line :: block_lines) i in
  Stream.from (next_block [])



(*let test = List.iter (print_char) (parse chlist)*)
