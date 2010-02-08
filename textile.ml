(* This file is part of textile-ocaml.
 *
 * textile-ocaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * textile-ocaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with textile-ocaml.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2010 Alexander Markov *)

open ExtLib

type attr =
  | Class    of string (* p(myclass). *)
  | Id       of string (* p(#myid). *)
  | Style    of string (* p{color:red}. *)
  | Language of string (* p[fr-fr]. *)
type phrase =
  | CData       of string
  | Emphasis    of (attr list * phrase list)   (* _ *)
  | Strong      of (attr list * phrase list)   (* * *)
  | Italic      of (attr list * phrase list)   (* __ *)
  | Bold        of (attr list * phrase list)   (* ** *)
  | Citation    of (attr list * phrase list)   (* ?? *)
  | Deleted     of (attr list * phrase list)   (* - *)
  | Inserted    of (attr list * phrase list)   (* + *)
  | Superscript of (attr list * phrase list)   (* ^ *)
  | Subscript   of (attr list * phrase list)   (* ~ *)
  | Span        of (attr list * phrase list)   (* % *)
  | Code        of (attr list * phrase list)   (* @ *)
  | Acronym of string * string                (* ABC(Always Be Closing *)
  | Image of attr list * string * string      (* !/fear.jpg(my wife)! *)
  | Link of (attr list * phrase list) *
      string option * string (* "linktext(title)":url *)
type line =
  phrase list
type talign =
  | Right   (** > *)
  | Left    (** < *)
  | Center  (** = *)
  | Justify (** <> *)
type valign =
  | Top    (** ^ *)
  | Middle (** - *)
  | Bottom (** ~ *)
type padding =
  int * int
type options =
  attr list * talign option * padding
type cellspan =
  int option * int option
type tableoptions =
  options * valign option
type celltype =
  | Data
  | Head
type cell =
  (celltype * tableoptions * cellspan) * line list
type row =
  tableoptions * cell list
type block =
  | Header     of int * (options * line list) (** h1. *)
  | Blockquote of (options * line list)       (** bq. *)
  | Footnote   of int * (options * line list) (** fnn. *)
  | Paragraph  of (options * line list)     (** p. *)
  | Blockcode  of (options * string list)   (** bc. *)
  | Pre        of (options * string list)   (** pre. *)
  | Numlist    of (options * line list)     (** # *)
  | Bulllist   of (options * line list)     (** * *)
  | Table      of (tableoptions * row list) (** |t|a|b|l|e| *)

(* There are internal exceptions. They must be even catched
 * inside the module *)
exception Invalid_modifier
exception Invalid_attribute
exception Invalid_row

type block_modifier =
  | BHeader     of int * (options * (bool * int))
  | BBlockquote of (options * (bool * int))
  | BFootnote   of int * (options * (bool * int))
  | BParagraph  of (options * (bool * int))
  | BBlockcode  of (options * (bool * int))
  | BPre        of (options * (bool * int))
  | BNumlist    of (options * (bool * int))
  | BBulllist   of (options * (bool * int))
  | BTable      of (tableoptions * row)

type encasing_char =
  | Blank
  | Brace

let num_of_char c =
  (int_of_char c) - 48

(* find_from str sub start returns the character number of the first
 * occurence of sstring sub in string str after position start. Raises
 * Not_found if there are no such substring after position start. *)
let find_from str sub start =
  let sublen = String.length sub in
  let sublast = sublen - 1 in
  let rec loop n m =
    try
      if str.[n+m] = sub.[m] then
        if sublast = m then n
        else loop n (m+1)
      else loop (n+1) 0
    with Invalid_argument _ -> raise Not_found in
  loop start 0

let rec njunk stream n =
  if n > 0
  then
    (Stream.junk stream;
    njunk stream (n-1))
  else ()

let rec peekn stream n =
  let l = Stream.npeek (n+1) stream in
  try Some (List.nth l n)
  with Failure _ -> None

let parse_stream stream =

  let defaultoptions = ([], None, (0, 0)) in
  let defaulttableoptions = (defaultoptions, None) in
  let defaultcelloptions = (Data, defaulttableoptions, (None, None)) in

  let get_attr str n =
    (* Extracts an attribute which closes by char c *)
    let extr_attr n c constr =
    (try
      let e = String.index_from str (n+1) c in
      Some (constr (String.sub str (n+1) (e-n-1)), (e+1))
    with
     (* If we have an open parenthesis and some happened shit *)
      Not_found | Invalid_argument _ -> None) in
    match str.[n] with
    (* Style *)
    | '{' -> extr_attr n '}' (fun x -> Style x)
    (* This may be a class, an id or left padding *)
    | '(' -> (match str.[n+1] with
        | '#' -> extr_attr (n+1) ')' (fun x -> Id x)
        |  _  -> extr_attr n ')' (fun x -> Class x))
    (* Language *)
    | '[' -> extr_attr n ']' (fun x -> Language x)
    |  _ -> None in

  let get_padding str n (lpad, rpad) =
    match str.[n] with
    | ')' -> Some ((lpad, rpad+1), n+1)
    | '(' -> Some ((lpad+1, rpad), n+1)
    |  _  -> None in

  let get_talign str n talign =
    match str.[n], talign with
    | '<', None -> (match str.[n+1] with
        | '>' -> Some (Justify, n+2)
        |  _  -> Some (Left, n+1))
    | '>', None -> Some (Right, n+1)
    | '=', None -> Some (Center, n+1)
    |  _ -> None in

  let get_celltype str n celltype =
    match str.[n], celltype with
    | '_', Data -> Some (Head, n+1)
    | _ -> None in

  let get_valign str n valign =
    match str.[n], valign with
    | '^', None -> Some (Top,    n+1)
    | '-', None -> Some (Middle, n+1)
    | '~', None -> Some (Bottom, n+1)
    | _ -> None in

  let get_option str n (attrs, talign, ((lp, rp) as padding)) =
    (match get_attr str n with
    | Some (attr, n) -> Some (((attr::attrs), talign, padding), n)
    | None ->
    (match get_padding str n padding with
    | Some ((l, r), n) -> Some ((attrs, talign, (lp+l, rp+r)), n)
    | None ->
    (match get_talign str n talign with
    | Some (talign, n) -> Some ((attrs, (Some talign), padding), n)
    | None -> None))) in

  let get_tableoption str n (options, valign) =
    (match get_option str n options with
    | Some (options, n) -> Some ((options, valign), n)
    | None ->
    (match get_valign str n valign with
    | Some (valign, n) -> Some ((options, (Some valign)), n)
    | None -> None)) in

  let get_phrase_attrs str start =
    let rec loop acc n =
      try
        match get_attr str n with
        | Some (attr, n) -> loop (attr::acc) n
        | None -> acc, n
      with Invalid_argument _ -> [], start
    in loop [] start in

  let get_block_opts str start =
    let rec loop options n =
      try
        (match get_option str n options with
        | Some (options, n) -> loop options n
        | None ->
        (match str.[n] with
        | '.' -> (match str.[n+1] with
            | ' ' -> false, options, n+2
            | '.' -> (match str.[n+2] with
                | ' ' -> true, options, n+3
                |  _  -> raise Invalid_modifier)
            |  _  -> raise Invalid_modifier)
        |  _  -> raise Invalid_modifier))
      with Invalid_argument _ -> raise Invalid_modifier in
    loop ([], None, (0,0)) start in

  let get_cell_opts str start =
    let rec loop (celltype, tableoptions, cellspan) n =
      try
        (match get_celltype str n celltype with
        | Some (celltype, n) -> loop (celltype, tableoptions, cellspan) n
        | None ->
        (match get_tableoption str n tableoptions with
        | Some (tableoptions, n) ->
            loop (celltype, tableoptions, cellspan) n
        | None ->
        (match str.[n] with
        | '.' -> (match str.[n+1] with
            | ' ' -> (celltype, tableoptions, cellspan), (n+2)
            |  _  -> defaultcelloptions, start)
        |  _  -> defaultcelloptions, start)))
      with Invalid_argument _ -> defaultcelloptions, start in
    loop defaultcelloptions start in

  let get_row_options str start =
    let rec loop tableoptions n =
      (match get_tableoption str n tableoptions with
      | Some (tableoptions, n) -> loop tableoptions n
      | None ->
      (match str.[n] with
      | '.' -> (match str.[n+1] with
          | ' ' -> tableoptions, (n+2)
          |  _  -> defaulttableoptions, start)
      |  _  -> defaulttableoptions, start)) in
    loop defaulttableoptions start in

  let get_table_opts str start =
    let rec loop tableoptions n =
      try
        (match get_tableoption str n tableoptions with
        | Some (tableoptions, n) -> loop tableoptions n
        | None ->
        (match str.[n] with
        | '.' when (n+1) = (String.length str) -> tableoptions
        | _ -> raise Invalid_modifier))
      with Invalid_argument _ -> raise Invalid_modifier in
    loop defaulttableoptions start in

  let rec parse_string str =
    let is_blank = function
      | ' ' | '\t' -> true
      | _ -> false in
    let enc_char c =
      if is_blank c then Some Blank
      else if c = '[' then Some Brace
      else None in
    let is_final_enc str n = function
      | Blank when String.length str <= n -> true
      | Blank when is_blank str.[n] -> true
      | Brace when str.[n] = ']' -> true
      | _ -> false in

    (* Cut empty phrases *)
    if String.length str = 0 then [] else
    let pack_cdata str start len =
      CData (String.sub str start len) in
    let rec find_modifier prev_char n =
      try
        let myfunc n t =
          let cm = close_modifier n t in
          let ps = parse_string in
          match str.[n], str.[n+1] with
          | '_', '_' -> cm (n+2) "__" (fun x -> Italic x)
          | '_',  _  -> cm (n+1) "_"  (fun x -> Emphasis x)
          | '*', '*' -> cm (n+2) "**" (fun x -> Bold x)
          | '*',  _  -> cm (n+1) "*"  (fun x -> Strong x)
          | '?', '?' -> cm (n+2) "??" (fun x -> Citation x)
          | '-',  _  -> cm (n+1) "-"  (fun x -> Deleted x)
          | '+',  _  -> cm (n+1) "+"  (fun x -> Inserted x)
          | '^',  _  -> cm (n+1) "^"  (fun x -> Superscript x)
          | '~',  _  -> cm (n+1) "~"  (fun x -> Subscript x)
          | '%',  _  -> cm (n+1) "%"  (fun x -> Span x)
          | '@',  _  -> cm (n+1) "@"  (fun x -> Code x)
          (*| '!',  _  -> cm (n+1) "!"  (fun x -> Image    ([], x, ""))*)
          | '"',  _  -> close_link n t
          | _ -> find_modifier str.[n] (n+1) in
        match enc_char prev_char with
        | Some t -> myfunc n t
        | None -> find_modifier str.[n] (n+1)
      (* If we have passed whole string without any modifier
       * then we simply pack it in CData *)
      with Invalid_argument _ -> [CData str]
                    (* End of last lexeme position
                     * vvvv *)
    and close_modifier eoll enc_type start cstr constr =
      if str.[start] = ' ' then find_modifier ' ' (start+1) else
      let attrs, start = get_phrase_attrs str start in
      let rec loop n =
        try
          let pos = find_from str cstr n in
          if is_final_enc str (pos+(String.length cstr)) enc_type then
              let k = match enc_type with Brace -> 1 | _ -> 0 in
              let parsed_phrase = constr (attrs,
                parse_string (String.sub str
                  start
                  (pos-start))) in
              let postfix =
                parse_string (
                  let s = pos + (String.length cstr) + k in
                  String.sub str s ((String.length str) - s)
                ) in
              let tail =
                parsed_phrase :: postfix in
              (* Fixes empty strings in lines like ^"_some line_"$ *)
              if eoll = 0
              then tail
              else (pack_cdata str 0 (eoll - k)) :: tail
          else loop (pos+1)
        with Not_found -> find_modifier str.[start-1] start in
      loop start
    and close_link linkstart enc_type =
      try
        let pos = find_from str "\":" linkstart in
        let textend = (pos-1) in
        let rec loop n =
          if is_final_enc str n enc_type
          then
            (let k = match enc_type with Brace -> 1 | _ -> 0 in
            let parsed_phrase =
              Link (([], parse_string
              (String.sub str (linkstart+1) (textend-linkstart))), None,
              String.sub str (textend+3) (n-textend-3)) in
            let postfix =
              parse_string (
                let s = n + k in
                String.sub str s ((String.length str) - s)
              ) in
            let tail =
              parsed_phrase :: postfix in
            if linkstart = 0
            then tail
            else (pack_cdata str 0 (linkstart - k)) :: tail)
          else loop (n+1) in
        loop (textend+1)
      with Not_found -> find_modifier str.[linkstart] (linkstart+1) in
    find_modifier ' ' 0 in

  let get_celllines str peeks start =
    let rec loop str acc peeks start n =
      try
        match str.[n] with
        | '|' ->
            let cellstring = String.sub str (start) (n - start) in
            let cellline = parse_string cellstring in
            Some (List.rev (cellline::acc), str, peeks, (n+1))
        |  _  ->
            loop str acc peeks start (n+1)
      with Invalid_argument _ ->
        (match n with
        | 0 -> raise Invalid_row
        | n when start = n -> None
        | n ->
            let cellstring = String.sub str (start) (n - start) in
            let cellline = parse_string cellstring in
            (match peekn stream peeks with
            | Some nextstr ->
                loop nextstr (cellline::acc) (peeks+1) 0 0
            | None -> raise Invalid_row)) in
    loop str [] peeks start start in

  let get_row peeks str =
    if String.length str > 0
    then
      let toptions, start =
        get_row_options str 0 in
      let rec move n =
        try
          (match str.[n] with
          | '|' ->
              (let rec loop str acc peeks start =
                let celloptions, start =
                  get_cell_opts str start in
                (match get_celllines str peeks start with
                | Some (celllines, str, peeks, start) ->
                    loop str ((celloptions, celllines)::acc) peeks start
                | None ->
                    njunk stream peeks;
                    (toptions, List.rev acc)) in
              loop str [] peeks (start+1))
          | ' ' | '\t' -> move (n+1)
          | _ -> raise Invalid_row)
        with Invalid_argument _ -> raise Invalid_row in
      move start
    else raise Invalid_row in

  let get_rows frow =
    let rec loop acc =
      match Stream.peek stream with
      | Some str ->
          (try
            let row = get_row 1 str in
            loop (row::acc)
          with Invalid_row -> List.rev acc)
      | None -> List.rev acc in
    loop [frow] in

  let get_block_modifier fstr =
    let options start =
      let is_ext, options, start = get_block_opts fstr start in
      options, (is_ext, start) in
    try
      Some (match fstr.[0], fstr.[1], fstr.[2] with
        (* Headers  *)
        | 'h', c,  _
          when (c >= '0') && (c <= '6') ->
            BHeader ((num_of_char c), options 2)
        | 'b','q', _  ->
            BBlockquote (options 2)
        | 'f','n', c  ->
            (* It just works
             * I don't know how *)
            let check x = (x >= 0) && (x <= 9) in
            let rec loop acc n =
              let num = num_of_char fstr.[n] in
              if check num
              then loop ((acc*10)+num) (n+1)
              else
                BFootnote (acc, options n) in
            let num = num_of_char fstr.[2] in
            if check num
            then loop num 3
            else raise Invalid_modifier
        | 'b','c', _  ->
            BBlockcode (options 2)
        | 'p','r','e' ->
            BPre (options 3)
        | 'p', _,  _  ->
            BParagraph (options 1)
        | _ ->
            (try
              (*if String.starts_with fstr "table"
              then
                let tableoptions, start = get_table_opts fstr 5 in
                let nextstr = peek
                TableWithAttrs (tableoptions, get_row 5 fstr)
              else*) BTable (defaulttableoptions, get_row 0 fstr)
            with Invalid_row | Invalid_modifier -> raise Invalid_modifier))
    with
      (* If our string is too shorter... *)
      | Invalid_argument _
      | Invalid_modifier -> None in

  let get_func parsing_func fstr is_ext start =
    let rec loop acc =
      try
        let str = Stream.next stream in
        match str, is_ext with
        | "", false -> List.rev acc
        | "", true -> (match Stream.peek stream with
            | Some next_str ->
                (match get_block_modifier next_str with
                | Some _ -> List.rev acc
                | None   ->
                    let result = parsing_func str in
                    loop (result::acc))
            | None -> List.rev acc)
        | str, _ ->
            let result = parsing_func str in
            loop (result::acc)
      with Stream.Failure -> List.rev acc in
    let first_line =
      parsing_func (String.sub fstr start ((String.length fstr) - start)) in
    loop [first_line] in

  let get_block fstr =
    match get_block_modifier fstr with
    | Some (block_modifier) ->
        let get_lines (is_ext, start) =
          get_func parse_string fstr is_ext start in
        let get_strings (is_ext, start) =
          get_func (fun x -> x) fstr is_ext start in
        (match block_modifier with
        | BHeader (n, (o, t)) -> Header (n, (o, get_lines t))
        | BBlockquote (o, t)  -> Blockquote (o, get_lines t)
        | BFootnote (n, (o, t)) -> Footnote (n, (o, get_lines t))
        | BParagraph (o, t) -> Paragraph    (o, get_lines t)
        | BBlockcode (o, t) -> Blockcode    (o, get_strings t)
        | BPre       (o, t) -> Pre          (o, get_strings t)
        | BNumlist   (o, t) -> Numlist      (o, get_lines t)
        | BBulllist  (o, t) -> Bulllist     (o, get_lines t)
        | BTable (topts, frow) -> Table (topts, get_rows frow))
    | None ->
        Paragraph (([], None, (0,0)), get_func parse_string fstr false 0) in

  let rec next_block () =
    try
      match Stream.next stream with
      |  ""  -> next_block ()
      | fstr -> Some (get_block fstr)
    with Stream.Failure -> None in

  Stream.from (fun _ -> next_block ())
