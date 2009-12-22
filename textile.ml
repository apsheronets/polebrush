open ExtLib

type attr =
  | Class    of string (* p(myclass). *)
  | Id       of string (* p(#myid). *)
  | Style    of string (* p{color:red}. *)
  | Language of string (* p[fr-fr]. *)
type phrase =
  | CData       of string
  | Emphasis    of phrase list (* _ *)
  | Strong      of phrase list (* * *)
  | Italic      of phrase list (* __ *)
  | Bold        of phrase list (* ** *)
  | Citation    of phrase list (* ?? *)
  | Deleted     of phrase list (* - *)
  | Inserted    of phrase list (* + *)
  | Superscript of phrase list (* ^ *)
  | Subscript   of phrase list (* ~ *)
  | Span        of phrase list (* % *)
  | Code        of phrase list (* @ *)
  | Acronym of string * string (* ABC(Always Be Closing *)
  | Link of string * phrase    (* "linktext":url *)
type line =
  phrase list
type align =
  | Right   (* > *)
  | Left    (* < *)
  | Center  (* = *)
  | Justify (* <> *)
type block =
  | Header     of int * (attr list * align option * line list) (* h1. *)
  | Blockquote of (attr list * align option * line list) (* bq. *)
  | Footnote   of int * (attr list * align option * line list) (* fnn. *) (* FIXME *)
  | Paragraph  of (attr list * align option * line list) (* p. *)
  | Blockcode  of (attr list * align option * line list) (* bc. *)
  | Pre        of (attr list * align option * line list) (* pre. *)
  | Numlist    of line list (* # *)
  | Bulllist   of line list (* * *)
  (*| Table of FIXME *)


exception Parse_failure

let junk n s =
  let rec loop = function
    | 0 -> ()
    | t -> Stream.junk s; loop (t-1)
  in loop n

let sub str start =
  String.sub str start (String.length str - start)

let num_of_char c =
  (int_of_char c) - 48

let parse_stream stream =
  let rec line_of_string str =
    (* Cut empty phrases *)
    if String.length str = 0 then [] else
    let pack_cdata str start len =
      CData (String.sub str start len) in
    let rec find_modifier prev_char n =
      try
        match prev_char, str.[n] with
        | ' ', '_' -> (* FIXME: need italic support *)
            close_modifier (n+1) (n-1) ['_'] (fun x -> Emphasis x)
        | ' ', '*' -> (* FIXME: need bold support *)
            close_modifier (n+1) (n-1) ['*'] (fun x -> Strong x)
        (* FIXME: need citation support *)
        | ' ', '-' ->
            close_modifier (n+1) (n-1) ['-'] (fun x -> Deleted x)
        | ' ', '+' ->
            close_modifier (n+1) (n-1) ['+'] (fun x -> Inserted x)
        | ' ', '^' -> (* FIXME: is it a good implementation? *)
            close_modifier (n+1) (n-1) ['^'] (fun x -> Superscript x)
        | ' ', '~' ->
            close_modifier (n+1) (n-1) ['~'] (fun x -> Subscript x)
        | ' ', '%' ->
            close_modifier (n+1) (n-1) ['%'] (fun x -> Span x)
        | ' ', '@' ->
            close_modifier (n+1) (n-1) ['@'] (fun x -> Code x)
        | _, c -> find_modifier c (n+1)
      (* If we have passed whole string without any modifier
       * then we simply pack it in CData *)
      with Invalid_argument _ -> [CData str]
                          (* End of last lexeme position
                           * vvvv *)
    and close_modifier start eoll char_list constr =
      (* don't forget what chlist is not the same as char_list! *)
      let rec loop clist n =
        try
          match str.[n], clist with
          | c, [h]  when c = h ->
              (* PLEASE FIXME *)
              pack_cdata str 0 (eoll+1)
              :: constr (line_of_string (
                String.sub str start (n + 1 - start - (List.length clist))
              ))
              :: line_of_string (
                let s = n + (List.length clist) in
                String.sub str s ((String.length str) - s)
              )
          | c, h::t when c = h -> loop t (n+1)
          | _ -> loop clist (n+1)
        with Invalid_argument _ -> (*CData str (* FIXME *)*)
          match char_list with
          (* This branch ... *) (* Goddamn, FOR WHAT did I create *)
          (*| [] -> *)          (* this branch? *)
          (* FAIL *)
          | _  -> find_modifier str.[start-1] start in
      loop char_list start in
    find_modifier ' ' 0 in
  let lines_of_strings strings =
    List.map (line_of_string) strings in
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
            | ' ' -> constr (attrs, align, lines_of_strings (sub f (n+2) :: t))
            |  _  -> raise Parse_failure)
        |  _ -> Paragraph ([], align, (lines_of_strings lines))
      with Parse_failure | Invalid_argument _ ->
        Paragraph ([], align, (lines_of_strings lines))
    (* Extracts an attribute which closes by char c *)
    and extract_attr_and_continue n c constr attrs align =
      (try
        let e = String.index_from f (n+1) c in
        let result = constr (String.sub f (n+1) (e-n-1)) in
        loop (result :: attrs) align (e+1)
      with (* If we have an open parenthesis and some happened shit
        * then we stop to parse and leave string "s" as is *)
        Not_found | Invalid_argument _ -> raise Parse_failure)
    in loop [] None start in
  let parse_block strings =
    let f = List.hd strings in (* first string *)
    let t = List.tl strings in (* all another strings *)
    try
      match f.[0], f.[1], f.[2] with
        (* Headers  *)
        | 'h', c,  _
          when ((num_of_char c) >= 0)
            && ((num_of_char c) <= 6) ->
            parse_lines 2 (fun x -> Header ((num_of_char c), x)) strings
        | 'b','q', _  ->
            parse_lines 2 (fun x -> Blockquote x) strings
        (* FIXME: footnote support needed *)
        (*| 'f','n', _  ->
            (  )*)
        | 'b','c', _  ->
            parse_lines 2 (fun x -> Blockcode x) strings
        | 'p','r','e' ->
            parse_lines 3 (fun x -> Pre x) strings
        | 'p', _,  _  ->
            parse_lines 1 (fun x -> Paragraph x) strings
        (* Simple paragraph if nothing else is matching *)
        | _ -> Paragraph ([], None, (lines_of_strings (f::t)))
    (* Simple paragraph if our string is too shorter *)
    with Invalid_argument _ ->
      Paragraph ([], None, (lines_of_strings (f::t))) in
  let rec next_block block_strings i =
    match Stream.peek stream, block_strings with
      | None, [] ->
          None
      | Some "", [] ->
          Stream.junk stream;
          next_block block_strings i
      | Some "", _ | None, _ ->
          Some (parse_block (List.rev block_strings))
      | Some str, _ ->
          Stream.junk stream;
          next_block (str :: block_strings) i in
  Stream.from (next_block [])
