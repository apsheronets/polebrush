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
        try Some (input_line channel)
        with End_of_file -> None) in
  line_stream_of_channel channel

let s = read_file "test.txt"

let junk n s =
  let rec loop = function
    | 0 -> ()
    | t -> Stream.junk s; loop (t-1)
  in loop n

let parse lines =
  let parse_phrase s =
    CData s in
  let parse_block (f::t) =
    let sub start str =
      String.sub str start ((String.length str)-start) in
    (* parse phrase from... *)
    let p start =
      parse_phrase (sub start f :: t) in
    match f.[0], f.[1], f.[2], f.[3], f.[4] with
    | 'h','1','.',' ', _  ->
        Header1 (p 4)
    | 'h','2','.',' ', _  ->
        Header2 (p 4)
    | 'h','3','.',' ', _  ->
        Header3 (p 4)
    | 'b','q','.',' ', _  ->
        Blockquote (p 4)
    | 'f','n', c, '.',' ' -> (* FIXME *)
        Footnote (p 5)
    | 'b','c','.',' ', _  ->
        Blockcode (p 4)
    | 'p','r','e','.',' ' ->
        Pre (p 4)
    | 'p','.',' ', _,  _  ->
        Paragraph (p 3)
    | _ ->
        Paragraph (parse_phrase (f :: t)) in
  let rec next_block block_lines i =
    match Stream.peek lines, block_lines with
      | None, [] ->
          None
      | Some "", [] ->
          Stream.junk lines;
          next_block block_lines i
      | Some "", _ | None, _ ->
          Some (parse_block (List.rev block_lines))
      | Some line, _ ->
          Stream.junk lines;
          next_block (line :: block_lines) i in
  Stream.from (next_block [])



(*let test = List.iter (print_char) (parse chlist)*)
