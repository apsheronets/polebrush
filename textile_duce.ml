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
 * Copyright 2011 Alexander Markov *)

open Printf
open Textile
open Xhtmltypes_duce

(* Ocamlduce's crutches *)
let utf = Ocamlduce.Utf8.make
let xmlfold f1 f2 l =
    try
      let h = List.hd l in
      let t = List.tl l in
      let rec fold f accu l =
        match l with
        | [] -> accu
        | a::l -> fold f (f accu (f1 a)) l in
      fold f2 (f1 h) t
    with Failure _ -> raise (Failure "xmlfold")

exception Invalid_textile of string

let xhtml_of_block =

  let parse_attr = function
    | Class    s -> {{ { class=(utf s) } }}
    | Id       s -> {{ {    id=(utf s) } }}
    | Style    s -> {{ { style=(utf s) } }}
    | Language s -> {{ {  lang=(utf s) } }} in

  let parse_attrs =
    List.fold_left
    (fun (acc : {{ attrs }} ) attr ->
        {{ acc ++ (parse_attr attr) }} )
      {{ {} }} in
  let pa = parse_attrs in

  let rec parse_phrase_without_links =
    let pl = parse_line in function
    | CData str         -> {{ (utf str) }}
    | Strong      (a,l) -> {{ [ <strong (pa a)>(pl l) ] }}
    | Italic      (a,l) -> {{ [ <i      (pa a)>(pl l) ] }}
    | Bold        (a,l) -> {{ [ <b      (pa a)>(pl l) ] }}
    | Emphasis    (a,l) -> {{ [ <em     (pa a)>(pl l) ] }}
    | Citation    (a,l) -> {{ [ <cite   (pa a)>(pl l) ] }}
    | Deleted     (a,l) -> {{ [ <del    (pa a)>(pl l) ] }}
    | Inserted    (a,l) -> {{ [ <ins    (pa a)>(pl l) ] }}
    | Superscript (a,l) -> {{ [ <sup    (pa a)>(pl l) ] }}
    | Subscript   (a,l) -> {{ [ <sub    (pa a)>(pl l) ] }}
    | Span        (a,l) -> {{ [ <span   (pa a)>(pl l) ] }}
    | Code        (a,s) -> {{ [ <code   (pa a)>(utf s) ] }}
    | Notextile      s  -> {{ (utf s) }}
    | Acronym (a, b) ->
        {{ [ <acronym title=(utf b)>(utf a) ] }}
    | Image (a, float, src, alt) ->
        let alt = match alt with
        | Some s -> {{ {alt=(utf s)} }}
        | None -> {{ {alt=""} }} in
        let float = match float with
        | Some Float_left  -> {{ {style={:"float: left" :}} }}
        | Some Float_right -> {{ {style={:"float: right":}} }}
        | None -> {{ {} }} in
        {{ [ <img ((pa a) ++ {src=(utf src)} ++ alt ++ float)>[] ] }}
    | Link _        -> raise (Invalid_textile "unexpected link")
    | Reference i ->
        let t = utf (sprintf "%d" i) in
        let fn_link = utf (sprintf "#fn%d" i) in
        let ref_id  = utf (sprintf "ref%d" i) in
        {{ [<sup class="footnote">[<a id=ref_id href=fn_link>t]] }}

  and parse_phrase = function
    | Link ((attrs, l), title, url) ->
        let title = (match title with
          | Some s -> {{ {title=(utf s)} }}
          | None -> {{ {} }}) in
        {{ [ <a ((pa attrs) ++ title ++ {href=(utf url)})>
          (parse_line_without_links l) ] }}
    | x -> parse_phrase_without_links x

  and parse_line_without_links line =
    List.fold_left
      (fun (acc : {{ a_contents }}) phrase ->
        {{ acc @ (parse_phrase_without_links phrase) }} )
      {{ [] }} line

  and parse_line line =
    List.fold_left
      (fun (acc : {{ inlines }}) phrase ->
        {{ acc @ (parse_phrase phrase) }} )
      {{ [] }} line in

  let parse_lines =
    xmlfold
      (fun l -> parse_line l)
      (fun (acc : {{ inlines }} ) l ->
        {{ acc @ [<br>[]] @ l }} ) in

  let parse_strings =
    xmlfold
      (fun str -> utf str)
      (fun (acc : {{ [Char*] }} ) x ->
        {{ acc @ "\n" @ x }} ) in

  let parse_talign = function
    | Some talign ->
      (let s = match talign with
        | Right   -> "right"
        | Left    -> "left"
        | Center  -> "center"
        | Justify -> "justify" in
      {{ { style={:"text-align:"^s:} } }})
    | None        -> {{ {}  }} in

  let parse_padding = function
    | 0, 0 -> {{ {} }}
    | l, 0 ->
        {{ { style={:sprintf "padding-left:%uem" l:} } }}
    | 0, r ->
        {{ { style={:sprintf "padding-right:%uem" r:} } }}
    | l, r ->
        {{ { style={:sprintf
          "padding-left:%uem;padding-right:%uem" l r:} } }} in

  let parse_options (attrs, talign, padding) =
    {{ (parse_attrs attrs)
    ++ (parse_talign talign)
    ++ (parse_padding padding) }} in

  let parse_valign = function
    | Some x ->
        (let s = match x with
        | Top -> "top"
        | Middle -> "middle"
        | Bottom -> "bottom" in
        {{ { style={:"vertical-align:"^s:} } }})
    | None -> {{ {} }} in

  let parse_tableoptions (opts, valign) =
    {{ (parse_options opts)
    ++ (parse_valign valign) }} in

  let po = parse_options in
  let pt = parse_tableoptions in

  let parse_cells =
    xmlfold
      (fun ((celltype, topts, cellspan), lines) ->
        let topts = pt topts in
        match celltype with
        | Data -> {{ [<td (topts)>(parse_lines lines)] }}
        | Head -> {{ [<th (topts)>(parse_lines lines)] }})
      (fun (acc : {{ [(th|td)+] }} ) x ->
        {{ acc @ x }} ) in

  let parse_rows =
    xmlfold
      (fun (topts, cells) -> {{ [<tr (pt topts)>(parse_cells cells)] }} )
      (fun (acc : {{ [tr+] }} ) x ->
        {{ acc @ x }} ) in

  let parse_list (f: {{ [li+] }} -> {{ ol|ul }} ) =
    let rec fill_lvl
        filled_lvl
        (prev:flows)
        (acc:{{[li*]}})
        : Textile.element list -> ( {{ [li+] }} * Textile.element list) =
      function
      | (lvl, line) :: t when lvl = filled_lvl ->
          fill_lvl filled_lvl (parse_line line) {{ acc @ [<li>prev] }} t
      | (lvl, line) :: t when lvl = filled_lvl + 1 ->
          let first = parse_line line in
          let lis, rest = fill_lvl lvl first {{ [] }} t in
          fill_lvl filled_lvl {{ [!prev (f lis)] }} acc rest
      | ((lvl, _) :: t) as l when lvl < filled_lvl ->
          {{ acc @ [<li>prev] }}, l
      | [] as l ->
          {{ acc @ [<li>prev] }}, l
      | (lvl, _) :: _ ->
          raise (Invalid_textile (
            sprintf "strange bull- or numlist: filled level is %d, but the next element has level %d"
              filled_lvl lvl)) in
    function
    | [] -> raise (Invalid_textile "empty bull- or numlist")
    | (1, line)::t ->
        let first = parse_line line in
        let lis, _ = fill_lvl 1 first {{ [] }} t in
        f lis
    | _ -> raise (Invalid_textile "strange bull- or numlist") in

  function
    | Header (i, (opts, lines)) ->
        (match i with
        | 1 -> {{ <h1 (po opts)>(parse_lines lines) }}
        | 2 -> {{ <h2 (po opts)>(parse_lines lines) }}
        | 3 -> {{ <h3 (po opts)>(parse_lines lines) }}
        | 4 -> {{ <h4 (po opts)>(parse_lines lines) }}
        | 5 -> {{ <h5 (po opts)>(parse_lines lines) }}
        | 6 -> {{ <h6 (po opts)>(parse_lines lines) }}
        | _ -> raise (Invalid_textile "incorrect header level"))
    | Blockquote (opts, lines) ->
        {{ <blockquote (po opts)>[<p (po opts)>(parse_lines lines)] }}
    | Footnote (i, (opts, lines)) ->
        let ref_url = utf (sprintf "#ref%d" i) in
        let arrow = utf "↑" in
        {{ <p ({id={:"fn" ^ string_of_int i:} class="footnote"}
          ++ (po opts))>[<sup>(utf (string_of_int i))
          ' ' <a href=ref_url>arrow ' ' !(parse_lines lines)] }}
    | Paragraph (opts, lines) ->
        {{ <p (po opts)>(parse_lines lines) }}
    | Blockcode (opts, strings) ->
        {{ <pre ({class="blockcode"} ++ (po opts))>[
          <code>(parse_strings strings)] }}
    | Pre (opts, strings) ->
        {{ <pre (po opts)>(parse_strings strings) }}
    | Blocknott (opts, strings) ->
        {{ <div (po opts)>(parse_strings strings) }}
    | Numlist  elements ->
        parse_list (fun lis -> {{ <ol>lis }}) elements
    | Bulllist elements ->
        parse_list (fun lis -> {{ <ul>lis }}) elements
    | Table (topts, rows) ->
        {{ <table (pt topts)>(parse_rows rows) }}

let xhtml_of_textile stream =
  let rec loop (acc : blocks) =
    try
      let block = Stream.next stream in
      loop {{ acc @ [(xhtml_of_block block)] }}
    with Stream.Failure -> acc in
  loop {{ [] }}
