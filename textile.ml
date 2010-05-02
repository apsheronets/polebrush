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
open Printf

type attr =
  | Class    of string (* p(myclass). *)
  | Id       of string (* p(#myid). *)
  | Style    of string (* p{color:red}. *)
  | Language of string (* p[fr-fr]. *)
type phrase =
  | CData       of string
  | Emphasis    of (attr list * phrase list) (* _ *)
  | Strong      of (attr list * phrase list) (* * *)
  | Italic      of (attr list * phrase list) (* __ *)
  | Bold        of (attr list * phrase list) (* ** *)
  | Citation    of (attr list * phrase list) (* ?? *)
  | Deleted     of (attr list * phrase list) (* - *)
  | Inserted    of (attr list * phrase list) (* + *)
  | Superscript of (attr list * phrase list) (* ^ *)
  | Subscript   of (attr list * phrase list) (* ~ *)
  | Span        of (attr list * phrase list) (* % *)
  | Code        of (attr list * phrase list) (* @ *)
  | Acronym of string * string               (* ABC(Always Be Closing *)
  | Image of attr list * string * string option (* !imgsrc(alt)! *)
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
  | Head (* _ *)
type celloptions =
  celltype * valign option * cellspan
type cell =
  celloptions * line list
type row =
  tableoptions * cell list
type element =
  int * line
type block =
  | Header     of int * (options * line list) (** h1. *)
  | Blockquote of (options * line list)       (** bq. *)
  | Footnote   of int * (options * line list) (** fnn. *)
  | Paragraph  of (options * line list)     (** p. *)
  | Blockcode  of (options * string list)   (** bc. *)
  | Pre        of (options * string list)   (** pre. *)
  | Numlist    of (options * element list)  (** # *)
  | Bulllist   of (options * element list)  (** * *)
  | Table      of (tableoptions * row list) (** |t|a|b| *)

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
  | BNumlist    of (options * element)
  | BBulllist   of (options * element)
  | BTable      of (tableoptions * row)

type encasing_char =
  | Blank
  | Brace

let num_of_char c =
  (int_of_char c) - 48

let str_end str start =
  String.sub str start ((String.length str) - start)

let substr str s e =
  String.sub str s (e - s)

(* find_from str sub start returns the character number of the first
 * occurence of string sub in string str after position start. Raises
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

(* Returns the substring between pos and the first occurence of
 * substring sub in string s and position in string after that
 * substring.
 * @raise Not_found if sub does not occur in s or received empty
 * substring. *)
let sub_before s pos sub =
  let cpos = find_from s sub pos in
  let res = substr s pos cpos in
  if (String.length res) = 0 then raise Not_found
  else (cpos + (String.length sub)), res

let of_stream stream =

  let defaultoptions = ([], None, (0, 0)) in
  let defaulttableoptions = (defaultoptions, None) in
  let defaultcelloptions = (Data, None, (None, None)) in

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
  let get_list_options str start =
    let rec loop options n =
      try
        (match get_option str n options with
        | Some (options, n) -> loop options n
        | None ->
        (match str.[n] with
        | ' ' -> options, n+1
        |  _  -> raise Invalid_modifier))
      with Invalid_argument _ -> raise Invalid_modifier in
    loop ([], None, (0,0)) start in

  let get_cell_opts str start =
    let rec loop (celltype, valign, cellspan) n =
      try
        (match get_celltype str n celltype with
        | Some (celltype, n) -> loop (celltype, valign, cellspan) n
        | None ->
        (match get_valign str n valign with
        | Some (valign, n) ->
            loop (celltype, Some valign, cellspan) n
        | None ->
        (match str.[n] with
        | '.' -> (match str.[n+1] with
            | ' ' -> (celltype, valign, cellspan), (n+2)
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
  (*let get_table_opts str start =
    let rec loop tableoptions n =
      try
        (match get_tableoption str n tableoptions with
        | Some (tableoptions, n) -> loop tableoptions n
        | None ->
        (match str.[n] with
        | '.' when (n+1) = (String.length str) -> tableoptions
        | _ -> raise Invalid_modifier))
      with Invalid_argument _ -> raise Invalid_modifier in
    loop defaulttableoptions start in*)

  (* Not tail recursive *)
  let rec parse_string str =
    let is_blank = function
      | ' ' | '\t' -> true
      | _ -> false in
    let is_punct c =
      try
        if (c >= '!' && c <= '.') || (c >= ':' && c <= '?') then true
        else false
      with Invalid_argument _ -> false in
    let enc_char c =
      if is_blank c then Some Blank
      else if c = '[' then Some Brace
      else None in
    let rec is_final_enc str n = function
      | Blank when String.length str <= n -> true
      | Blank when is_blank str.[n] -> true
      | Blank when is_punct str.[n] && is_final_enc str (n+1) Blank -> true
      | Brace when str.[n] = ']' -> true
      | _ -> false in
    let find_final_enc_from str start enc_type =
      let rec loop n =
        if is_final_enc str n enc_type
        then n
        else loop (n+1) in
      try
        loop start
      with Invalid_argument _ -> raise Not_found in
    let sub_before_enc s pos enc_type =
      let k = match enc_type with Brace -> 1 | _ -> 0 in
      let cpos = find_final_enc_from str pos enc_type in
      let res = substr s pos cpos in
      if (String.length res) = 0 then raise Not_found
      else cpos + k, res in
    let parse_next str beg phrase pos =
      let postfix =
        parse_string
          (str_end str beg) in
      let tail =
        phrase :: postfix in
      let prefix =
        String.sub str 0 pos in
      if String.length prefix = 0
      then tail
      else (CData prefix) :: tail in
    let get_title str =
      let last = (String.length str) - 1 in
      if str.[last] = ')' then
        try
          let titleend = last - 1 in
          let rec find_back str c n =
            if str.[n] = c then n else find_back str c (n-1) in
          let titlestart = (find_back str '(' (last-1)) + 1 in
          let strend = titlestart - 2 in
          if (titlestart = titleend) || (strend < 0)
          then str, None
          else substr str 0 (strend+1),
            Some (substr str titlestart (titleend+1))
        with Invalid_argument _ -> str, None
      else str, None in
    if String.length str = 0 then [] else (* Cut empty phrases *)
    let rec find_modifier prev_char n =
      try
        let f n t =
          let cm = close_modifier n t in
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
          | '!',  _  -> close_image (n+1) t
          | '"',  _  -> close_link  (n+1) t
          | _ -> find_modifier str.[n] (n+1) in
        match enc_char prev_char with
        | Some t -> f n t
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
              let phrase = constr (attrs,
                parse_string (substr str start pos)) in
              let postfix =
                parse_string
                  (let s = pos + (String.length cstr) + k in
                  str_end str s) in
              let tail =
                phrase :: postfix in
              (* Fixes empty strings in lines like ^"_some line_"$ *)
              if eoll = 0
              then tail
              else (CData (String.sub str 0 (eoll - k))) :: tail
          else loop (pos+1)
        with Not_found -> find_modifier str.[start-1] start in
      loop start
    and close_link start enc_type =
      try
        let k = match enc_type with Brace -> 1 | _ -> 0 in
        let urlstart, text_and_title = sub_before str start "\":" in
        let text, title = get_title text_and_title in
        let poststart, url = sub_before_enc str urlstart enc_type in
        let phrase =
          Link (([], parse_string text), title, url) in
        parse_next str (poststart + k) phrase (start - k - 1)
      with Not_found -> find_modifier str.[start] (start+1)
    and close_image start enc_type =
      try
        let k = match enc_type with Brace -> 1 | _ -> 0 in
        let pos, src_and_alt = sub_before str start "!" in
        let src, alt = get_title (src_and_alt) in
        let phrase, poststart =
          if is_final_enc str pos enc_type then
            Image ([], src, alt), pos
          else if str.[pos] = ':' then
            let poststart, url = sub_before_enc str (pos+1) enc_type in
            Link (([], [Image ([], src, alt)]), None, url),
              poststart
          else raise Not_found in
        parse_next str (poststart + k) phrase (start - k - 1)
      with Not_found -> find_modifier str.[start] (start+1) in
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

  let get_list_modifier str =
    match str.[0] with
    | '*' | '#' ->
      (let options, start = get_list_options str 1 in
      let line =
        parse_string (String.sub str start ((String.length str - start))) in
      match str.[0] with
      | '#' -> BNumlist  (options, (1, line))
      | '*' -> BBulllist (options, (1, line))
      | _ -> raise Invalid_modifier)
    | _ -> raise Invalid_modifier in
  let get_elements c felm =
    let rec get_level str n =
      try
        (match str.[n] with
        | ch  when ch = c -> get_level str (n+1)
        | ' ' when n > 0  -> Some n
        |  _  -> None)
      with Invalid_argument _ -> None in
    let rec loop acc =
      match Stream.peek stream with
      | Some str ->
        (match get_level str 0 with
        | Some n ->
            Stream.junk stream;
            loop ((n, (parse_string (str_end str (n+1)))) :: acc)
        | None -> List.rev acc)
      | None -> List.rev acc in
    loop [felm] in

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
            (try BTable (defaulttableoptions, get_row 0 fstr)
            with Invalid_row | Invalid_modifier ->
            get_list_modifier fstr))
    with
      | Invalid_argument _ (* ff our string is too short *)
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
        | BNumlist   (o, e) -> Numlist      (o, get_elements '#' e)
        | BBulllist  (o, e) -> Bulllist     (o, get_elements '*' e)
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

let list_of_stream stream =
  let rec loop acc =
    try
      let e = Stream.next stream in
      loop (e::acc)
    with Stream.Failure -> List.rev acc in
  loop []

let parse_list l =
  list_of_stream (of_stream (Stream.of_list l))

let xhtml_of_block ?(escape=true) block =
  let esc s =
    if escape then
      let strlen = String.length s in
      let buf = Buffer.create strlen in
      let f = function
        | '&' -> Buffer.add_string buf "&amp;"
        | '<' -> Buffer.add_string buf "&lt;"
        | '>' -> Buffer.add_string buf "&gt;"
        | '"' -> Buffer.add_string buf "&quot;"
        |  c  -> Buffer.add_char buf c in
      String.iter f s;
      Buffer.contents buf
    else s in

  let parse_attr = function
    | Class s    -> sprintf "class=%s" (esc s)
    | Id s       -> sprintf "id=%s" (esc s)
    | Style s    -> sprintf "style=%s" (esc s)
    | Language s -> sprintf "lang=%s" (esc s) in

  let parse_attrs = function
    | [] -> ""
    | attrs ->
        let buf = Buffer.create 80 in
        List.iter (fun attr ->
          Buffer.add_char buf ' ';
          Buffer.add_string buf (parse_attr attr)) attrs;
        Buffer.contents buf in
  let pa = parse_attrs in

  let rec parse_phrase =
    let p = sprintf in
    let pl = parse_line in function
    | CData str -> (esc str)
    | Strong      (a,l) -> p "<strong%s>%s</strong>" (pa a) (pl l)
    | Italic      (a,l) -> p "<i%s>%s</i>" (pa a) (pl l)
    | Bold        (a,l) -> p "<b%s>%s</b>" (pa a) (pl l)
    | Emphasis    (a,l) -> p "<em%s>%s</em>" (pa a) (pl l)
    | Citation    (a,l) -> p "<cite%s>%s</cite>" (pa a) (pl l)
    | Deleted     (a,l) -> p "<del%s>%s</del>" (pa a) (pl l)
    | Inserted    (a,l) -> p "<ins%s>%s</ins>" (pa a) (pl l)
    | Superscript (a,l) -> p "<sup%s>%s</sup>" (pa a) (pl l)
    | Subscript   (a,l) -> p "<sub%s>%s</sub>" (pa a) (pl l)
    | Span        (a,l) -> p "<span%s>%s</span>" (pa a) (pl l)
    | Code        (a,l) -> p "<code%s>%s</code>" (pa a) (pl l)
    | Acronym (a, b) ->
        p "<acronym title=\"%s\">%s</acronym>" (esc b) (esc a)
    | Image (a, src, alt) ->
        (let alt = match alt with
        | Some s -> p "alt=\"%s\"" (esc s)
        | None -> "alt=\"\"" in
        p "<img%s %s src=\"%s\" />" (pa a) alt (esc src))
    | Link ((attrs, l), title, url) ->
        (let title = match title with
          | Some s -> sprintf " title=%S" (esc s)
          | None -> "" in
        p "<a%s%s href=\"%s\">%s</a>" (pa attrs) title (esc url) (pl l))

  and parse_line line =
    String.concat "" (List.map parse_phrase line) in

  let parse_lines lines =
    String.concat "<br />" (List.map parse_line lines) in

  let parse_strings strings=
    String.concat "\n" (List.map esc strings) in

  let parse_talign = function
    | Some talign ->
        (let s = match talign with
        | Right   -> "right"
        | Left    -> "left"
        | Center  -> "center"
        | Justify -> "justify" in
        sprintf " style=\"text-align:%s" s)
    | None -> "" in

  let parse_padding = function
    | 0, 0 -> ""
    | l, 0 ->
        sprintf " style=\"padding-left:%dem\"" l
    | 0, r ->
        sprintf " style=\"padding-right:%dem\"" r
    | l, r ->
        sprintf " style=\"padding-left:%dem; padding-right:%dem\"" l r in

  let parse_options (attrs, talign, padding) =
    String.concat "" [parse_attrs attrs;
      parse_talign talign;
      parse_padding padding] in

  let parse_valign = function
    | Some x ->
        (let s = match x with
        | Top -> "top"
        | Middle -> "middle"
        | Bottom -> "bottom" in
        sprintf " style=\"vertical-align:%s\"" s)
    | None -> "" in

  let parse_tableoptions (opts, valign) =
    String.concat "" [parse_options opts; parse_valign valign] in

  let po = parse_options in
  let pt = parse_tableoptions in

  let parse_cells cells =
    String.concat "" (List.map (fun ((celltype, topts, cellspan), lines) ->
      let tag = match celltype with
      | Data -> "td" | Head -> "th" in
      (* FIXME: topts *)
      sprintf "<%s>%s<%s>" tag (parse_lines lines) tag) cells) in

  let parse_rows rows =
    String.concat "" (List.map (fun (topts, cells) ->
      sprintf "<tr%s>%s</tr>" (pt topts) (parse_cells cells)) rows) in

  let parse_list elements =
    (* FIXME: not correct *)
    String.concat "" (List.map (fun (level, line) ->
      sprintf "<li>%s</li>" (parse_line line)) elements) in

  let pl = parse_lines in
  match block with
  | Header (i, (opts, lines)) ->
      sprintf "<h%d%s>%s</h%d>" i (po opts) (pl lines) i
  | Blockquote (opts, lines) ->
      let popts = po opts in
      sprintf "<blockquote%s><p%s>%s</p></blockquote>"
        popts popts (pl lines)
  | Footnote (i, (opts, lines)) ->
      sprintf "<p id=\"fn%d\" class=\"footnote\"%s>%s</p>"
        i (po opts) (pl lines)
  | Paragraph (opts, lines) ->
      sprintf "<p%s>%s</p>" (po opts) (pl lines)
  | Blockcode (opts, strings) ->
      let popts = po opts in
      sprintf "<pre%s><code%s>%s</code></pre>"
        popts popts (parse_strings strings)
  | Pre (opts, strings) ->
      sprintf "<pre%s>%s</pre>"
        (po opts) (parse_strings strings)
  | Numlist (opts, elements) -> (* FIXME: opts *)
      sprintf "<ol>%s</ol>" (parse_list elements)
  | Bulllist (opts, elements) ->
      sprintf "<ul>%s</ul>" (parse_list elements)
  | Table (topts, rows) ->
      sprintf "<table%s>%s</table>" (pt topts) (parse_rows rows)

let to_xhtml ?(escape=true) stream =
  let next _ =
    try
      Some (xhtml_of_block ~escape (Stream.next stream))
    with Stream.Failure -> None in
  Stream.from next
