(* This file is part of polebrush.
 *
 * polebrush is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * polebrush is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with polebrush.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2011 Alexander Markov *)

open Printf
open Polebrush

(* code from Ocsigen
 * Copyright (C) 2005-2008 Vincent Balat, Stéphane Glondu
 * Laboratoire PPS - CNRS Université Paris Diderot *)

let hex_digits =
  [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
     '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]

let to_hex2 k =
  (* Converts k to a 2-digit hex string *)
  let s = String.create 2 in
  s.[0] <- hex_digits.( (k lsr 4) land 15 );
  s.[1] <- hex_digits.( k land 15 );
  s

let of_hex1 c =
  match c with
  | ('0'..'9') -> Char.code c - Char.code '0'
  | ('A'..'F') -> Char.code c - Char.code 'A' + 10
  | ('a'..'f') -> Char.code c - Char.code 'a' + 10
  | _ -> raise Not_found

(* http://www.w3schools.com/tags/att_standard_id.asp says:

Naming rules:

  * Must begin with a letter A-Z or a-z
  * Can be followed by: letters (A-Za-z), digits (0-9), hyphens ("-"), underscores ("_"), colons (":"), and periods (".")
  * Values are case-sensitive *)
let encode_id =
  let valid_char = function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | ':' | '.' -> true
    | _ -> false in
  let valid_begin_char = function
    | 'A'..'Z' | 'a'..'z' -> true
    | _ -> false in
  let esc s =
    let prefix = '.' in
    let strlen = String.length s in
    let buf = Buffer.create (strlen*6) in
    (try
      let () =
        let c = s.[0] in
        if valid_begin_char c
        then Buffer.add_char buf c
        else (
          Buffer.add_char   buf prefix;
          Buffer.add_string buf (to_hex2 (Char.code c))
        ) in
      for i = 1 to (String.length s - 1) do
        let c = s.[i] in
        if valid_char c
        then Buffer.add_char buf c
        else
          (* don't encode whitespaces *)
          if c = ' ' || c = '\t'
          then Buffer.add_char buf '-'
          else (
            Buffer.add_char   buf prefix;
            Buffer.add_string buf (to_hex2 (Char.code c));
          )
      done
    with Invalid_argument _ -> ());
    Buffer.contents buf in
  esc

let rec exude p = function
  | [] -> raise Not_found
  | x :: l -> (match p x with
      | Some y -> y
      | None -> exude p l)

let make_id_from_header lines =
  let strings = List.map string_of_line lines in
  let s = String.concat "\n" strings in
  (* FIXME *)
  encode_id s

exception Invalid_polebrush of string

type toc = (string * int * Polebrush.line list) list

let elements_of_toc : toc -> Polebrush.element list =
  List.map
    (fun (header_id, lvl, lines) ->
      lvl, [Polebrush.Link (([], [CData (String.concat " " (List.map Polebrush.string_of_line lines))]), None, sprintf "#%s" header_id)])

let esc s =
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
let dont_esc s = s

let parse_attr = function
  | Class    s -> sprintf "class=\"%s\"" (esc s)
  | Id       s -> sprintf "id=\"%s\""    (esc s)
  | Style    s -> sprintf "style=\"%s\"" (esc s)
  | Language s -> sprintf "lang=\"%s\""  (esc s)

let parse_attrs = function
  | [] -> ""
  | attrs ->
      let buf = Buffer.create 80 in
      List.iter (fun attr ->
        Buffer.add_char buf ' ';
        Buffer.add_string buf (parse_attr attr)) attrs;
      Buffer.contents buf
let pa = parse_attrs

let rec parse_phrase escape_cdata escape_nomarkup =
  let print_cdata    = if escape_cdata    then esc else dont_esc in
  let print_nomarkup = if escape_nomarkup then esc else dont_esc in
  let p = sprintf in
  let pl = of_line escape_cdata escape_nomarkup in function
  | CData str -> (print_cdata str)
  | Strong      (a,l) -> p "<strong%s>%s</strong>" (pa a) (pl l)
  | Italic      (a,l) -> p "<i%s>%s</i>"       (pa a) (pl l)
  | Bold        (a,l) -> p "<b%s>%s</b>"       (pa a) (pl l)
  | Emphasis    (a,l) -> p "<em%s>%s</em>"     (pa a) (pl l)
  | Citation    (a,l) -> p "<cite%s>%s</cite>" (pa a) (pl l)
  | Deleted     (a,l) -> p "<del%s>%s</del>"   (pa a) (pl l)
  | Inserted    (a,l) -> p "<ins%s>%s</ins>"   (pa a) (pl l)
  | Superscript (a,l) -> p "<sup%s>%s</sup>"   (pa a) (pl l)
  | Subscript   (a,l) -> p "<sub%s>%s</sub>"   (pa a) (pl l)
  | Span        (a,l) -> p "<span%s>%s</span>" (pa a) (pl l)
  | Code        (a,s) -> p "<code%s>%s</code>" (pa a) (esc s)
  | Nomarkup       s  -> p "%s" (print_nomarkup s)
  | Acronym (a, b) ->
      p "<acronym title=\"%s\">%s</acronym>" (esc b) (print_cdata a)
  | Image (a, float, src, alt) ->
      (let alt, title = match alt with
      | Some s -> let s = esc s in p "alt=\"%s\"" s, p " title=\"%s\"" s
      | None -> "alt=\"\"", "" in
      let float = match float with
      | Some Float_left  -> " style=\"float: left\""
      | Some Float_right -> " style=\"float: right\""
      | None -> "" in
      p "<img %s%s src=\"%s\"%s%s />" alt float (esc src) (pa a) title)
  | Link ((attrs, l), title, url) ->
      (let title = match title with
        | Some s -> sprintf " title=%S" (esc s)
        | None -> "" in
      p "<a%s href=\"%s\"%s>%s</a>" title (esc url) (pa attrs) (pl l))
  | Reference i ->
      p "<sup class=\"footnote\"><a id=\"ref%d\" href=\"#fn%d\">%d</a></sup>" i i i

and of_line escape_cdata escape_nomarkup line =
  String.concat "" (List.map (parse_phrase escape_cdata escape_nomarkup) line)

let of_block ?toc ?(escape_cdata=false) ?(escape_nomarkup=false) block =

  let print_nomarkup = if escape_nomarkup then esc else dont_esc in
  let of_line = of_line escape_cdata escape_nomarkup in

  let parse_lines lines =
    String.concat "<br />" (List.map of_line lines) in

  let to_lines print strings =
    String.concat "\n" (List.map print strings) in

  let parse_talign = function
    | Some talign ->
        (let s = match talign with
        | Right   -> "right"
        | Left    -> "left"
        | Center  -> "center"
        | Justify -> "justify" in
        sprintf " style=\"text-align:%s\"" s)
    | None -> "" in

  let parse_padding = function
    | 0, 0 -> ""
    | l, 0 ->
        sprintf " style=\"padding-left:%uem\"" l
    | 0, r ->
        sprintf " style=\"padding-right:%uem\"" r
    | l, r ->
        sprintf " style=\"padding-left:%uem; padding-right:%uem\"" l r in

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

  let parse_cells (cells : cell list) =
    String.concat "" (List.map (fun ((celltype, topts, (colspan, rowspan)), lines) ->
      let tag =
        match celltype with
        | Data -> "td"
        | Head -> "th" in
      let rowspan =
        match rowspan with
        | Some rowspan -> sprintf " rowspan=\"%d\"" rowspan
        | None -> "" in
      let colspan =
        match colspan with
        | Some colspan -> sprintf " colspan=\"%d\"" colspan
        | None -> "" in
      let topts = pt topts in
      let attrs = String.concat "" [topts; rowspan; colspan] in
      sprintf "<%s%s>%s</%s>" tag attrs (parse_lines lines) tag) cells) in

  let parse_rows (rows : Polebrush.row list) =
    String.concat "" (List.map (fun (topts, cells) ->
      sprintf "<tr%s>%s</tr>" (pt topts) (parse_cells cells)) rows) in

  let parse_list f =
    let rec fill_lvl filled_lvl prev acc =
      function
      | (lvl, line) :: t when lvl = filled_lvl ->
          fill_lvl filled_lvl (of_line line) (sprintf "%s<li>%s</li>" acc prev) t
      | (lvl, line) :: t when lvl = filled_lvl + 1 ->
          let first = of_line line in
          let lis, rest = fill_lvl lvl first "" t in
          fill_lvl filled_lvl (prev ^ (f lis)) acc rest
      | ((lvl, _) :: t) as l when lvl < filled_lvl ->
          sprintf "%s<li>%s</li>" acc prev, l
      | [] as l ->
          sprintf "%s<li>%s</li>" acc prev, l
      | (lvl, _) :: _ ->
          raise (Invalid_polebrush (
            sprintf "strange bull- or numlist: filled level is %d, but the next element has level %d"
              filled_lvl lvl)) in
    function
    | [] -> raise (Invalid_polebrush "empty bull- or numlist")
    | (1, line)::t ->
        let first = of_line line in
        let lis, _ = fill_lvl 1 first "" t in
        f lis
    | _ -> raise (Invalid_polebrush "strange bull- or numlist") in

  let pl = parse_lines in
  match block with
  | Header (i, (opts, lines)) ->
      sprintf "<h%d%s>%s</h%d>" i (po opts) (pl lines) i
  | Blockquote (opts, lines) ->
      let popts = po opts in
      sprintf "<blockquote%s><p%s>%s</p></blockquote>"
        popts popts (pl lines)
  | Footnote (i, (opts, lines)) ->
      sprintf "<p id=\"fn%d\" class=\"footnote\"%s><sup>%d</sup> <a href=\"#ref%d\">↑</a> %s</p>"
        i (po opts) i i (pl lines)
  | Paragraph (opts, lines) ->
      sprintf "<p%s>%s</p>" (po opts) (pl lines)
  | Blockcode (opts, strings) ->
      let popts = po opts in
      sprintf "<pre%s class=\"blockcode\"><code>%s</code></pre>"
        popts (to_lines esc strings)
  | Pre (opts, strings) ->
      sprintf "<pre%s>%s</pre>"
        (po opts) (to_lines esc strings)
  | Blocknomarkup (opts, strings) ->
      sprintf "<div%s>%s</div>"
        (po opts) (to_lines print_nomarkup strings)
  | Numlist  elements ->
      parse_list (sprintf "<ol>%s</ol>") elements
  | Bulllist elements ->
      parse_list (sprintf "<ul>%s</ul>") elements
  | Table (topts, rows) ->
      sprintf "<table%s>%s</table>" (pt topts) (parse_rows rows)
  | ToC (attrs, ta, p) ->
      (match toc with
      | None -> ""
      | Some toc ->
          let opts = (Class "toc" :: attrs), ta, p in
          let elements = elements_of_toc toc in
          let l = parse_list (fun x -> sprintf "<ol>%s</ol>" x) elements in
          sprintf "<div%s>%s</div>" (po opts) l)

let toc_of_enum enum =
  let toc_rev = ref [] in
  let id_tbl = Hashtbl.create 42 in
  let add_id attrs lvl lines =
    let header_id, add_to_attrs =
      (* try to find ready id *)
      try exude (function Id id -> Some id | _ -> None) attrs, false
      (* if not, make it from lines *)
      with Not_found -> make_id_from_header lines, true in
    let header_id, add_to_attrs =
      try
        let count = Hashtbl.find id_tbl header_id in
        let count = succ count in
        Hashtbl.replace id_tbl header_id count;
        sprintf "%s-%d" header_id count, true
      with Not_found ->
        (Hashtbl.add id_tbl header_id 0;
        header_id, add_to_attrs) in
    let attrs =
      if add_to_attrs
      then (Id header_id) :: attrs
      else attrs in
    toc_rev := (header_id, lvl, lines) :: !toc_rev;
    attrs in
  let toc_enum =
    Enum.map
      (function
        | Header (lvl, ((attrs,                  t, p), lines)) ->
          Header (lvl, ((add_id attrs lvl lines, t, p), lines))
        | b -> b)
      enum in
  let toc =
    Enum.force toc_enum;
    List.rev !toc_rev in
  toc, toc_enum

let of_enum ?(disable_toc=false) ?escape_cdata ?escape_nomarkup enum =
  let toc, enum =
    if disable_toc
    then None, enum
    else
      let toc, enum = toc_of_enum enum in
      (Some toc), enum in
  Enum.map (of_block ?toc ?escape_cdata ?escape_nomarkup) enum

let of_stream ?escape_cdata ?escape_nomarkup stream =
  Stream.from (fun _ ->
    try
      let b = Stream.next stream in
      Some (of_block ?escape_cdata ?escape_nomarkup b)
    with Stream.Failure -> None)

let of_line ?(escape_cdata=false) ?(escape_nomarkup=false) =
  of_line escape_cdata escape_nomarkup

