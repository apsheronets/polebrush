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
  | CData       of string
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
type line =
  phrase list
type align =
  | Right   (* > *)
  | Left    (* < *)
  | Center  (* = *)
  | Justify (* <> *)
type element =
  | Phrase  of phrase
  | Element of element
type block =
  | Header1    of (attr list * align option * line list) (* h1. *)
  | Header2    of (attr list * align option * line list) (* h2. *)
  | Header3    of (attr list * align option * line list) (* h3. *)
  | Blockquote of (attr list * align option * line list) (* bq. *)
  | Footnote   of (attr list * align option * line list) (* fnn. *) (* FIXME *)
  | Paragraph  of (attr list * align option * line list) (* p. *)
  | Blockcode  of (attr list * align option * line list) (* bc. *)
  | Pre        of (attr list * align option * line list) (* pre. *)
  | Numlist    of element list (* # *)
  | Bulllist   of element list (* * *)
  (*| Table of FIXME *)


let read_file file =
  let channel = open_in file in
  let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
        try Some (input_line channel)
        with End_of_file -> None) in
  line_stream_of_channel channel

let teststream = read_file "test.txt"

exception Parse_failure

let junk n s =
  let rec loop = function
    | 0 -> ()
    | t -> Stream.junk s; loop (t-1)
  in loop n

let sub str start =
  String.sub str start (String.length str - start)

let parse_stream lines =
  let parse_phrases strings =
    List.map (fun x -> [CData x]) strings in
  let parse_lines (start:int) (constr: 'a -> block) (lines: string list) =
    let f = List.hd lines in (* first line *)
    let t = List.tl lines in (* all another lines *)
    let rec loop attrs align n =
      try
        match f.[n], align with
        | '{', _ ->
            extract_attr_and_continue n '}' (fun x -> Style x) attrs align
        | '(', _ ->
            (* FIXME: doesn't support ids *)
            extract_attr_and_continue n ')' (fun x -> Class x) attrs align
        | '[', _ ->
            extract_attr_and_continue n ']' (fun x -> Language x) attrs align
        | '<', None -> (match f.[n+1] with
            | '>' -> loop attrs (Some Justify) (n+2)
            |  _  -> loop attrs (Some Left) (n+1))
        | '>', None -> loop attrs (Some Right) (n+1)
        | '=', None -> loop attrs (Some Justify) (n+1)
        | '.', _ -> (match f.[n+1] with
            | ' ' -> constr (attrs, align, parse_phrases (sub f (n+2) :: t))
            |  _  -> raise Parse_failure)
        |  _ -> Paragraph ([], align, (parse_phrases lines))
      with Parse_failure | Invalid_argument _ ->
        print_endline "oh shit!";
        Paragraph ([], align, (parse_phrases lines))
    (* Extracts an attribute which closes by char c *)
    and extract_attr_and_continue n c constr attrs align =
      (try
        let e = String.index_from f (n+1) c in
        let result = constr (String.sub f (n+1) (e-n-1)) in
        loop (result :: attrs) align (e+1)
      with (* If we have an open parenthesis and some happened shit
        * then we stop to parse and leave string "s" as is *)
        Not_found | Invalid_argument _ -> raise Parse_failure)
    in loop [] None start
  in
  let parse_block lines =
    let f = List.hd lines in (* first line *)
    let t = List.tl lines in (* all another lines *)
    try
      match f.[0], f.[1], f.[2] with
        | 'h','1',_ ->
            parse_lines 2 (fun x -> Header1 x) lines
        | 'p', _, _ ->
            parse_lines 1 (fun x -> Paragraph x) lines
        | _ -> Paragraph ([], None, (parse_phrases (f::t)))
    (* If string is too shorter *)
    with Invalid_argument _ ->
      Paragraph ([], None, (parse_phrases (f::t))) in
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


