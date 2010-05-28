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
    | Class str    -> {{ { class={:str:} } }}
    | Id str       -> {{ { id={:str:}    } }}
    | Style str    -> {{ { style={:str:} } }}
    | Language str -> {{ { lang={:str:} }  }} in

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
    | Code        (a,l) -> {{ [ <code   (pa a)>(pl l) ] }}
    | Acronym (a, b) ->
        (* FIXME: what the braces? *)
        {{ [ <acronym title=(utf b)>['(' !(utf a) ')'] ] }}
    | Image (a, src, alt) ->
        let alt = match alt with
        | Some s -> {{ {alt=(utf s)} }}
        | None -> {{ {alt=""} }} in
        {{ [ <img ((pa a) ++ {src={:src:}} ++ alt)>[] ] }}
    | Link _        -> raise (Invalid_textile "unexpected link")

  and parse_phrase = function
    | Link ((attrs, l), title, url) ->
        let title = (match title with
          | Some s -> {{ {title=(utf s)} }}
          | None -> {{ {} }}) in
        {{ [ <a ((pa attrs) ++ title ++ {href={:url:}})>
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
        {{ { style={:"padding-left:"^(string_of_int l)^"em":} } }}
    | 0, r ->
        {{ { style={:"padding-right:"^(string_of_int r)^"em":} } }}
    | l, r ->
        {{ { style={:"padding-left:"^(string_of_int l)^
          "em;padding-right:"^(string_of_int r)^"em":} } }} in

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

  let parse_cells cells =
    xmlfold
      (fun ((celltype, topts, cellspan), lines) ->
        (* FIXME: topts *)
        match celltype with
        | Data -> {{ [<td>(parse_lines lines)] }}
        | Head -> {{ [<th>(parse_lines lines)] }})
      (fun (acc : {{ [(th|td)+] }} ) x ->
        {{ acc @ x }} )
      cells in

  let parse_rows =
    xmlfold
      (fun (topts, cells) -> {{ [<tr (pt topts)>(parse_cells cells)] }} )
      (fun (acc : {{ [tr+] }} ) x ->
        {{ acc @ x }} ) in

  let parse_list elements =
    (* FIXME: not correct *)
    xmlfold
      (fun (level, line) -> {{ [<li>(parse_line line)] }})
      (fun (acc : {{ [li+] }} ) x ->
        {{ acc @ x }} )
      elements in

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
        {{ <p ({id={:"fn" ^ string_of_int i:} class="footnote"}
          ++ (po opts))>(parse_lines lines) }}
    | Paragraph (opts, lines) ->
        {{ <p (po opts)>(parse_lines lines) }}
    | Blockcode (opts, strings) ->
        {{ <pre (po opts)>[<code (po opts)>(parse_strings strings)] }}
    | Pre (opts, strings) ->
        {{ <pre (po opts)>(parse_strings strings) }}
    | Numlist (opts, elements) -> (* FIXME: opts *)
        {{ <ol>(parse_list elements) }}
    | Bulllist (opts, elements) ->
        {{ <ul>(parse_list elements) }}
    | Table (topts, rows) ->
        {{ <table (pt topts)>(parse_rows rows) }}

let xhtml_of_textile stream =
  let rec loop (acc : {{ [block*] }}) =
    try
      let block = Stream.next stream in
      loop {{ acc @ [(xhtml_of_block block)] }}
    with Stream.Failure -> acc in
  loop {{ [] }}

let xhtml_of_textile_list l =
  xhtml_of_textile (Stream.of_list l)
