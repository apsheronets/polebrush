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

(* find_from str sub start returns the character number of the first
 * occurence of string sub in string str after position start. Raises
 * Not_found if there are no such substring after position start. *)
let find_from str sub pos =
  let sublen = String.length sub in
  if sublen = 0 then
    0
  else
    let found = ref pos in
    let len = String.length str in
    try
      for i = pos to len - sublen do
        let j = ref 0 in
        while String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
          incr j;
          if !j = sublen then begin found := i; raise Exit; end;
        done;
      done;
      raise Not_found
    with
      Exit -> !found

let of_stream stream =
  let defaultoptions = ([], None, (0, 0)) in
  let defaulttableoptions = (defaultoptions, None) in
  let defaultcelloptions = (Data, None, (None, None)) in

  let num_of_char c =
    (int_of_char c) - 48 in

  (* Returns the substring between pos and the first occurence of
     substring sub in string s and position in string after that
     substring.
     @raise Not_found if sub does not occur in s or received empty
     substring. *)
  let sub_before s pos sub =
    let cpos = find_from s sub pos in
    let res = String.slice s ~first:pos ~last:cpos in
    if (String.length res) = 0 then raise Not_found
    else (cpos + (String.length sub)), res in

  (* junks n elements of the stream *)
  let rec njunk stream n =
    if n > 0
    then
      (Stream.junk stream;
      njunk stream (n-1))
    else () in
  (* returns n'th element of the stream *)
  let rec peekn stream n =
    let l = Stream.npeek (n+1) stream in
    try Some (List.nth l n)
    with Failure _ -> None in

  let get_attr str n =
    (* Extracts an attribute which closes by char c *)
    let extr_attr ?(blanks=true) n c constr =
      try
        let e = String.index_from str (n+1) c in
        let s = String.slice str ~first:(n+1) ~last:e in
        if not blanks && String.contains str ' '
        then None
        else Some (constr s, (e+1))
      with (* If we have an open parenthesis and some happened shit *)
        Not_found | Invalid_argument _ -> None in
    match str.[n] with
    (* Style *)
    | '{' -> extr_attr n '}' (fun x -> Style x)
    (* This may be a class, an id or left padding *)
    | '(' -> (match str.[n+1] with
        | '#' -> extr_attr (n+1) ')' (fun x -> Id x)
        |  _  -> extr_attr ~blanks:false n ')' (fun x -> Class x))
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

  (* Not tail recursive *)
  let rec to_line str =
    let is_blank = function
      | ' ' | '\t' -> true
      | _ -> false in
    let is_punct c =
      (c >= '!' && c <= '.') || (c >= ':' && c <= '?') in
    let enc_char c =
      if is_blank c || is_punct c then Some false
      else if c = '[' then Some true
      else None in
    let rec is_final_enc n brace =
      if brace then
        str.[n] = ']'
      else
        String.length str <= n ||
        is_blank str.[n] ||
        (is_punct str.[n] && is_final_enc (n+1) false) in
    let find_final_enc_from start brace =
      let rec loop n =
        if is_final_enc n brace
        then n
        else loop (n+1) in
      try
        loop start
      with Invalid_argument _ -> raise Not_found in
    let sub_before_enc pos brace =
      let k = if brace then 1 else 0 in
      let cpos = find_final_enc_from pos brace in
      let res = String.slice str ~first:pos ~last:cpos in
      if (String.length res) = 0 then raise Not_found
      else cpos + k, res in
    let parse_next beg phrase pos =
      let postfix =
        to_line
          (String.slice str ~first:beg) in
      let tail =
        phrase :: postfix in
      let prefix =
        String.slice str ~last:pos in
      (* cut empty phrases *)
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
          else String.slice str ~last:(strend+1),
            Some (String.slice str ~first:titlestart ~last:(titleend+1))
        with Invalid_argument _ -> str, None
      else str, None in
    (* Cut empty phrases *)
    if String.length str = 0 then [] else
    let rec find_modifier prev_char n =
      let start = n + 1 in
      let close_link brace =
        let attrs, textstart = get_phrase_attrs str start in
        let k = if brace then 1 else 0 in
        try
          let urlstart, text_and_title = sub_before str textstart "\":" in
          let text, title = get_title text_and_title in
          let poststart, url = sub_before_enc urlstart brace in
          let phrase =
            Link ((attrs, to_line text), title, url) in
          parse_next poststart phrase (start - k - 1)
        with Not_found -> find_modifier str.[start] (start+1) in
      let close_image brace =
        let attrs, srcstart = get_phrase_attrs str start in
        let k = if brace then 1 else 0 in
        try
          let pos, src_and_alt = sub_before str srcstart "!" in
          let src, alt = get_title (src_and_alt) in
          let phrase, poststart =
            if is_final_enc pos brace then
              Image (attrs, src, alt), pos
            else if str.[pos] = ':' then
              let poststart, url = sub_before_enc (pos+1) brace in
              Link (([], [Image (attrs, src, alt)]), None, url),
                poststart
            else raise Not_found in
          parse_next (poststart + k) phrase (start - k - 1)
        with Not_found -> find_modifier str.[start] (start+1) in
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
        | '!',  _  -> close_image t
        | '"',  _  -> close_link  t
        | _ -> find_modifier str.[n] (n+1) in
      try
        match enc_char prev_char with
        | Some t -> f n t
        | None -> find_modifier str.[n] (n+1)
      (* If we have passed whole string without any modifier
         then we simply pack it in CData *)
      with Invalid_argument _ -> [CData str]
                    (* End of last lexeme position
                     * vvvv *)
    and close_modifier eoll brace start cstr constr =
      (* oops, blank space *)
      if is_blank str.[start] then find_modifier str.[start] (start+1) else
      let attrs, start = get_phrase_attrs str start in
      let rec loop n =
        try
          let pos = find_from str cstr n in
          if is_final_enc (pos+(String.length cstr)) brace then
            let k = if brace then 1 else 0 in
            let phrase = constr (attrs,
              to_line (String.slice str ~first:start ~last:pos)) in
            let postfix =
              to_line
                (let first = pos + (String.length cstr) + k in
                String.slice str ~first) in
            let tail =
              phrase :: postfix in
            (* Fixes empty strings in lines like ^"_some line_"$ *)
            if eoll = 0
            then tail
            else (CData (String.slice str ~last:(eoll - k))) :: tail
          else loop (pos+1)
        with Not_found -> find_modifier str.[start-1] start in
      loop start in
    find_modifier ' ' 0 in

  let get_celllines str peeks start =
    let rec loop str acc peeks start n =
      try
        match str.[n] with
        | '|' ->
            let cellstring = String.slice str ~first:start ~last:n in
            let cellline = to_line cellstring in
            Some (List.rev (cellline::acc), str, peeks, (n+1))
        |  _  ->
            loop str acc peeks start (n+1)
      with Invalid_argument _ ->
        (match n with
        | 0 -> raise Invalid_row
        | n when start = n -> None
        | n ->
            let cellstring = String.slice str ~first:start ~last:n in
            let cellline = to_line cellstring in
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
        to_line (String.slice str ~first:start) in
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
            loop ((n, (to_line (String.slice str ~first:(n+1)))) :: acc)
        | None -> List.rev acc)
      | None -> List.rev acc in
    loop [felm] in

  let get_block_modifier fstr =
    let options start =
      let is_ext, options, start = get_block_opts fstr start in
      options, (is_ext, start) in
    try
      Some (match fstr.[0], fstr.[1], fstr.[2] with
        | 'h', c,  _
          when (c >= '0') && (c <= '6') ->
            BHeader ((num_of_char c), options 2)
        | 'b','q', _  ->
            BBlockquote (options 2)
        | 'f','n', _  ->
            let rec loop n =
              if fstr.[n] >= '0' && fstr.[n] <= '9'
              then loop (n+1)
              else n in
            if fstr.[2] >= '0' && fstr.[2] <= '9'
            then
              let n = loop 3 in
              let i = int_of_string (String.slice fstr ~first:2 ~last:n) in
              BFootnote (i, options n)
            else raise Invalid_modifier
        | 'b','c', _  ->
            BBlockcode (options 2)
        | 'p','r','e' ->
            BPre (options 3)
        | 'p', _,  _  ->
            BParagraph (options 1)
        | _ ->
            (try if String.starts_with fstr "table"
            then begin
              let topts = get_table_opts fstr 5 in
              match Stream.peek stream with
              | None -> raise Invalid_modifier
              | Some s ->
                  let frow = get_row 0 s in
                  Stream.junk stream;
                  BTable (topts, frow)
            end else raise Invalid_modifier
            with Invalid_row | Invalid_modifier ->
            try BTable (defaulttableoptions, get_row 0 fstr)
            with Invalid_row | Invalid_modifier ->
            get_list_modifier fstr))
    with
      | Invalid_argument _ (* too short *)
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
      parsing_func (String.slice fstr ~first:start) in
    loop [first_line] in

  let get_block fstr =
    match get_block_modifier fstr with
    | Some (block_modifier) ->
        let get_lines (is_ext, start) =
          get_func to_line fstr is_ext start in
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
        Paragraph (([], None, (0,0)), get_func to_line fstr false 0) in

  let rec next_block () =
    try
      match Stream.next stream with
      |  ""  -> next_block ()
      | fstr -> Some (get_block fstr)
    with Stream.Failure -> None in

  Stream.from (fun _ -> next_block ())

let rec string_of_line line =
  let buf = Buffer.create 512 in
  let add = Buffer.add_string buf in
  let rec loop = function
    | h::t -> (match h with
        | CData s -> add s
        | Emphasis    (_, l)
        | Strong      (_, l)
        | Italic      (_, l)
        | Bold        (_, l)
        | Citation    (_, l)
        | Deleted     (_, l)
        | Inserted    (_, l)
        | Superscript (_, l)
        | Subscript   (_, l)
        | Span        (_, l)
        | Code        (_, l) ->
            add (string_of_line l)
        | Acronym (a, _) ->
            add a
        | Image _ -> ()
        | Link ((_, l), _, _) ->
            add (string_of_line l));
        loop t
    | [] -> Buffer.contents buf in
  loop line
