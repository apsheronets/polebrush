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

open Printf
open Textile

exception Invalid_textile of string

let of_block ?(escape=true) block =
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
    Buffer.contents buf in
  let esc_cdata = if escape then esc else (fun s -> s) in

  let parse_attr = function
    | Class s    -> sprintf "class=\"%s\"" (esc s)
    | Id s       -> sprintf "id=\"%s\"" (esc s)
    | Style s    -> sprintf "style=\"%s\"" (esc s)
    | Language s -> sprintf "lang=\"%s\"" (esc s) in

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
    | CData str -> (esc_cdata str)
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
        p "<acronym title=\"%s\">%s</acronym>" (esc b) (esc_cdata a)
    | Image (a, float, src, alt) ->
        (let alt = match alt with
        | Some s -> p "alt=\"%s\"" (esc s)
        | None -> "alt=\"\"" in
        let float = match float with
        | Some Float_left  -> " style=\"float: left\""
        | Some Float_right -> " style=\"float: right\""
        | None -> "" in
        p "<img %s src=\"%s\"%s%s />" alt (esc src) (pa a) float)
    | Link ((attrs, l), title, url) ->
        (let title = match title with
          | Some s -> sprintf " title=%S" (esc s)
          | None -> "" in
        p "<a%s href=\"%s\"%s>%s</a>" title (esc url) (pa attrs) (pl l))

  and parse_line line =
    String.concat "" (List.map parse_phrase line) in

  let parse_lines lines =
    String.concat "<br />" (List.map parse_line lines) in

  let to_lines strings =
    String.concat "\n" (List.map esc strings) in

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
    String.concat "" (List.map (fun ((celltype, topts, cellspan), lines) ->
      let tag = match celltype with
      | Data -> "td" | Head -> "th" in
      let topts = pt topts in
      sprintf "<%s%s>%s</%s>" tag topts (parse_lines lines) tag) cells) in

  let parse_rows (rows : Textile.row list) =
    String.concat "" (List.map (fun (topts, cells) ->
      sprintf "<tr%s>%s</tr>" (pt topts) (parse_cells cells)) rows) in

  let parse_list f =
    let rec fill_lvl filled_lvl prev acc =
      function
      | (lvl, line) :: t when lvl = filled_lvl ->
          fill_lvl filled_lvl (parse_line line) (sprintf "%s<li>%s</li>" acc prev) t
      | (lvl, line) :: t when lvl = filled_lvl + 1 ->
          let first = parse_line line in
          let lis, rest = fill_lvl lvl first "" t in
          fill_lvl filled_lvl (prev ^ (f lis)) acc rest
      | ((lvl, _) :: t) as l when lvl < filled_lvl ->
          sprintf "%s<li>%s</li>" acc prev, l
      | [] as l ->
          sprintf "%s<li>%s</li>" acc prev, l
      | (lvl, _) :: _ ->
          raise (Invalid_textile (
            sprintf "strange bull- or numlist: filled level is %d, but the next element has level %d"
              filled_lvl lvl)) in
    function
    | [] -> raise (Invalid_textile "empty bull- or numlist")
    | (1, line)::t ->
        let first = parse_line line in
        let lis, _ = fill_lvl 1 first "" t in
        f lis
    | _ -> raise (Invalid_textile "strange bull- or numlist") in

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
      sprintf "<pre%s class=\"blockcode\"><code>%s</code></pre>"
        popts (to_lines strings)
  | Pre (opts, strings) ->
      sprintf "<pre%s>%s</pre>"
        (po opts) (to_lines strings)
  | Numlist  elements ->
      parse_list (sprintf "<ol>%s</ol>") elements
  | Bulllist elements ->
      parse_list (sprintf "<ul>%s</ul>") elements
  | Table (topts, rows) ->
      sprintf "<table%s>%s</table>" (pt topts) (parse_rows rows)

let of_stream ?(escape=true) stream =
  let next _ =
    try
      Some (of_block ~escape (Stream.next stream))
    with Stream.Failure -> None in
  Stream.from next

