open ExtLib

(* my loveley *)
let (@@) = List.rev_append
let (>>) f g = g f


(* * * * * * * * * * *
 * Type  declaration *
 * * * * * * * * * * *)

type attr =
  | Class    of string (* p(myclass). *)
  | Id       of string (* p(#myid). *)
  | Style    of string (* p{color:red}. *)
  | Language of string (* p[fr-fr]. *)
type phrase =
  | CData       of string list
  | Emphasis    of phrase (* _ *)
  | Strong      of phrase (* * *)
  | Italic      of phrase (* __ *)
  | Bold        of phrase (* ** *)
  | Citation    of phrase (* ?? *)
  | Deleted     of phrase (* - *)
  | Inserted    of phrase (* + *)
  | Superscript of phrase (* ^ *)
  | Subscript   of phrase (* ~ *)
  | Span        of phrase (* % *)
  | Code        of phrase (* @ *)

  | Link of string * phrase
type alignment =
  | Right   (* > *)
  | Left    (* < *)
  | Center  (* = *)
  | Justify (* <> *)
type element =
  | Phrase  of phrase
  | Element of element
type block =
  | Header1 of (attr list * phrase)        (* h1. *)
  | Header2 of phrase        (* h2. *)
  | Header3 of phrase        (* h3. *)
  | Blockquote of phrase     (* bq. *)
  | Footnote of phrase       (* fnn. *) (* FIXME *)
  | Paragraph of (attr list * phrase) (* p. *)
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

exception Parse_failure

let junk n s =
  let rec loop = function
    | 0 -> ()
    | t -> Stream.junk s; loop (t-1)
  in loop n

let parse lines =
  let parse_attrs s =
    let rec loop acc n =
      try
        match s.[n] with
        | '{' ->
            extract_attr_and_continue n '}' (fun x -> Style x) acc
        | '(' ->
            (* FIXME: doesn't support ids *)
            extract_attr_and_continue n ')' (fun x -> Class x) acc
        | '[' ->
            extract_attr_and_continue n ']' (fun x -> Language x) acc
        |  _  -> acc
      with Invalid_argument _ -> acc
    and extract_attr_and_continue n c constr acc =
      (try
        let e = String.index_from s (n+1) c in
        let result = constr (String.sub s (n+1) (e-n-1)) in
        loop (result :: acc) (e+1)
      with (* If we have an open parenthesis and some happened shit
        * then we stop to parse and leave string "s" with peace *)
        Not_found | Invalid_argument _ -> raise Parse_failure)
    in loop [] 0
  in
  let parse_phrase s =
    CData s in
  let parse_block (f::t) =
    let sub start str =
      String.sub str start ((String.length str)-start) in
    try
    (* the first occurrence of a dot *)
    let dot = String.index f '.' in
    let parse_attrs_and_phrase start constr =
      (try
        constr (parse_attrs (String.sub f start (dot-1)), parse_phrase ((sub (dot+1) f)::t))
      with Parse_failure ->
        Paragraph ([] ,(parse_phrase (f::t)))) in
    match f.[0], f.[1], f.[2] with
      | 'h','1',_ ->
          parse_attrs_and_phrase 2 (fun x -> Header1 x)
      | 'p', _, _ ->
          parse_attrs_and_phrase 1 (fun x -> Paragraph x)
      | _ -> Paragraph ([] ,(parse_phrase (f::t)))
    (* If there is no dots in sting *)
    with Not_found -> Paragraph ([], (parse_phrase (f::t))) in
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
