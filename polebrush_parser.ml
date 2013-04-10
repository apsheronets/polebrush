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

open ExtLib
open Polebrush
open Polebrush_parsercomb

let (>>) f g = g f
let ($) a b = fun x -> a (b x)


(* some defaults *)

let default_options = ([], None, (0, 0))
let default_tableoptions = (default_options, None)
let default_celloptions = (Data, default_tableoptions, (None, None))
let empty_line = []


(* various helpers *)

let num_of_char c = (int_of_char c) - 48

(* junks n elements of the stream *)
let rec njunk stream n =
  if n > 0
  then begin Stream.junk stream; njunk stream (n-1) end

(* returns n'th element of the stream (from zero) *)
let rec peekn stream n =
  let l = Stream.npeek (n+1) stream in
  try Some (List.nth l n)
  with Failure _ | ExtList.List.Invalid_index _ -> (* ExtLib, goddamn *)
    None


(* let's parse *)

let p_string_not_empty = function "" -> fail | s -> return s
let whitespace = function ' ' | '\t' -> true | _ -> false
let punct = function
  | '!' | '"' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | ':' | ';' | '<' | '=' | '>' | '?' -> true | _ -> false
  (*(c >= '!' && c < '#') || (c > '#' && c <= '.') || (c >= ':' && c <= '?')*)
let p_whitespace = p_pred whitespace
let p_not_whitespace = p_pred (fun c -> not (whitespace c))
let p_punct = p_pred punct

(* checks previous char; doesn't jump *)
let check_prev p (s, pos) =
  let prev_pos = pos - 1 in
  (p >>= fun r -> fun _ -> Parsed (r, (s, pos))) (s, prev_pos)

(* checks current char; doesn't jump *)
let check_current p (s, pos) =
  (p >>= fun r -> fun _ -> Parsed (r, (s, pos))) (s, pos)

(* parses all kinds of declarations include (classname#id1#id2) *)
let class_and_ids =
  (* ((())) must be for padding, not for class (( or something else *)
  p_char '(' >>>
  p_until (p_pred ((<>) '(')) (p_char ')') >>=
  fun (s, _) ->
    match String.nsplit s "#" with
    | [] -> fail
    | "" :: ids ->
        return (List.map (fun x -> Id x) ids)
    | classname :: ids ->
        return ((Class classname) :: (List.map (fun x -> Id x) ids))
let style    = p_char '{' >>> p_str_until (p_char '}') >>= p_string_not_empty
let language = p_char '[' >>> p_str_until (p_char ']') >>= p_string_not_empty

let attr_decl =
  class_and_ids |||
  (style    >>= fun s -> return [Style    s]) |||
  (language >>= fun s -> return [Language s])

(*let attrs =
  p_manyf attr (fun acc x -> x::acc) [] in*)

(* this is for correct parsing strings like _(hi)_ *)
let try_attrs f =
  (p_seq attr_decl >>= (return $ List.flatten) >>= f) |||
  (*(p_plusf attr (fun acc x -> x::acc) [] >>= f) |||*)
  (f [])

let img_float =
  (p_char '<' >>> return Float_left)  |||
  (p_char '>' >>> return Float_right)

(* attributes + floating *)
let img_opts =
  let add_opt (attrs, float_opt) = function
    | `Attr a -> (a @ attrs, float_opt)
    | `Img_float f -> (attrs, Some f) in
  p_manyf
    ((attr_decl >>= fun a -> return (`Attr a)) ||| (img_float >>= fun f -> return (`Img_float f)))
    add_opt
    ([], None)

(* matches typical beginning of phrase: beginning of line or whitespace *)
let begin_of_phrase begin_of_line follow =
  (* why so unobvious solution? We can write it in that way:
   * begin_of_phrase begin_of_line =
   *   p_pos begin_of_line ||| p_whitespace || p_punct
   * but it willn't parse strings like (@code@) because it will detect
   * the begin of line, then found '(' which is not a modifier and all
   * parser fails. *)
  (p_pos begin_of_line >>> follow) |||
  (
    ((p_whitespace) |||
    (p_pred (function '(' | '\'' | '"' -> true | _ -> false))) >>> follow
  )

(* matches typical end of phrase: end of line, whitespace, punctuation
 * doesn't jump *)
let end_of_phrase =
  dont_jump
    (p_end |||
      (p_whitespace >>> return ()) |||
      (p_many p_punct >>> (p_end ||| (p_whitespace >>> return ()))))

(* The Great Function which collects CData and more interesting
 * phrases into line *)
(* it fails if [until] not reached *)
let collect_phrases_with phrase until (s, begin_of_line) =
  let rec loop acc beg (s, pos) =
    let go_on () = loop acc beg (s, succ pos) in
    match phrase (s, pos) with
    | Parsed ((phrase_r, last_cdata_pos), (s, next_p)) ->
        let acc_values =
          (* do we have some cdata to save which was
           * before we found a phrase? *)
          if last_cdata_pos <= beg
          then
            [phrase_r]
          else
            let prev_cdata =
              CData (String.slice ~first:beg ~last:last_cdata_pos s) in
            [prev_cdata; phrase_r] in
        loop (List.rev_append acc_values acc) next_p (s, next_p)
    | Failed  ->
        (match until (s, pos) with
        | Parsed (until_r, (s, new_pos)) ->
            if pos = begin_of_line
            then go_on ()
            else
              let acc =
                (* do we have some cdata to save which was
                 * before we found a termination combinator? *)
                if beg = pos
                then acc
                else
                  let last_cdata =
                    CData (String.slice ~first:beg ~last:pos s) in
                  last_cdata::acc in
                Parsed ((List.rev acc, until_r), (s, new_pos))
        | Failed ->
            if pos >= String.length s
            then
              (* we have passed the whole string
               * and haven't catch a termination combinator *)
              Failed
            else go_on ()) in
  loop [] begin_of_line (s, begin_of_line)

let phrase_surrounding end_of_phrase beg_of_line phrase =
  (* phrases are usually surrounded with whitespaces, punctuation,
   * begining/ending of line —
   * every case described in begin_of_phrase *)
  (
    begin_of_phrase beg_of_line (
      current_pos >>= fun last_cdata_pos ->
      phrase end_of_phrase >>= fun r ->
      return (r, last_cdata_pos))
  )
  |||
  (* but phrases can also be surrounded with square brackets *)
  (
    (* XXX: this makes code about 4x faster *)
    (*current_pos >>= fun last_cdata_pos ->
    p_char '[' >>>
    phrases last_cdata_pos (p_char ']' >>> return ())*)
    p_char '[' >>>
    current_pos >>= fun _pos ->
    phrase (p_char ']' >>> return ()) >>= fun r ->
    return (r, (_pos-1))
  )

(* separetely from other stuff — references *)
let reference beg_of_line =
  (p_not_whitespace >>> current_pos) >>= fun e ->
  p_char '[' >>>
  p_unsign_int >>= fun i ->
  p_char ']' >>> end_of_phrase >>>
  return ((Reference i), e)

(** high level function which made for collecting phrases
    @param what phrases to parse; everything else is CData
    @param ended_with what can be at the end of phrase
    @param from where to start
    @param until end when this parser matched *)
let collect ~what ~ended_with ~from ~until =
  collect_phrases_with
    ((phrase_surrounding
      ended_with
      from
      what) ||| reference from)
    until

(* Hyprlinks can't contain another hyperlinks.
 * Therefore, there are two functions for parsing phrases —
 * one without hyperlinks... *)
let rec phrases_except_hyperlinks end_of_phrase =
  (* opened modifier should not be before whitespace *)
  let opened_modifier m =
    m >>= fun r -> check_current p_not_whitespace >>> return r in
  (* and closed modifier also should not be after whitespace *)
  let closed_modifier m =
    check_prev p_not_whitespace >>> m >>> end_of_phrase in
  (* there are general definition of simple phrases *)
  let sp modifier =
    opened_modifier modifier >>= fun (f, cm) ->
    try_attrs (fun a ->
    current_pos >>= fun from ->
    (* FIXME *)
    let until = closed_modifier cm in
    collect
      ~what:all_phrases
      ~ended_with:(end_of_phrase ||| (dont_jump until >>> return ()))
      ~from
      ~until >>= fun (line, _) ->
    return (f (a, line))) in
  (* remember that __ and ** must be first than _ and * *)
  (* simple_phrase (p_str "__") (fun x -> Italic      x) |||
     simple_phrase (p_char '_') (fun x -> Emphasis    x) |||
     simple_phrase (p_str "**") (fun x -> Bold        x) |||
     simple_phrase (p_char '*') (fun x -> Strong      x) |||
     simple_phrase (p_str "??") (fun x -> Citation    x) |||
     simple_phrase (p_char '-') (fun x -> Deleted     x) |||
     simple_phrase (p_char '+') (fun x -> Inserted    x) |||
     simple_phrase (p_char '^') (fun x -> Superscript x) |||
     simple_phrase (p_char '~') (fun x -> Subscript   x) |||
     simple_phrase (p_char '%') (fun x -> Span        x) |||
     simple_phrase (p_char '@') (fun x -> Code        x) |||*)
  sp (p_str "__" >>> return ((fun x -> Italic      x), p_str "__")) |||
  sp (p_str "**" >>> return ((fun x -> Bold        x), p_str "**")) |||
  sp (p_pred2 (function
    | '_' -> Some (((fun x -> Emphasis    x), p_char '_'))
    | '*' -> Some (((fun x -> Strong      x), p_char '*'))
    | '-' -> Some (((fun x -> Deleted     x), p_char '-'))
    | '+' -> Some (((fun x -> Inserted    x), p_char '+'))
    | '^' -> Some (((fun x -> Superscript x), p_char '^'))
    | '~' -> Some (((fun x -> Subscript   x), p_char '~'))
    | '%' -> Some (((fun x -> Span        x), p_char '%'))
    | _ -> None)) |||
  sp (p_str "??" >>> return ((fun x -> Citation    x), p_str "??")) |||
  (* and there are not too simple phrases *)
  (* code *)
  (
    opened_modifier (p_char '@') >>>
    try_attrs (fun a ->
    p_str_until (closed_modifier (p_char '@')) >>= fun s ->
    return (Code (a, s)))
  ) |||
  (* nomarkup *)
  (
    opened_modifier (p_str "==") >>>
    p_str_until (closed_modifier (p_str "==")) >>= fun s ->
    return (Nomarkup s)
  ) |||
  (* image *)
  (
    (* ...:http://komar.bitcheese.net *)
    let link_opt =
      (p_char ':' >>>
        p_until (p_not_whitespace) end_of_phrase >>= fun (url, _) ->
        return (Some url)) |||
      (end_of_phrase >>> return None) in
    (* ...(title)! *)
    let end_with_title =
      p_char '(' >>>
      p_str_until (p_str ")!") >>= fun title ->
      link_opt >>= fun link_opt ->
      return (title, link_opt) in
    (* ...! *)
    let end_with_no_title =
      p_char '!' >>>
      link_opt in

    p_char '!' >>>
    img_opts >>= fun (attrs, float) ->
    p_until p_not_whitespace (
      (end_with_title >>= fun (title, link_opt) -> return (Some title, link_opt)) |||
      (end_with_no_title >>= fun link_opt -> return (None, link_opt))
    ) >>= fun (src, (title_opt, link_opt)) ->

    let r =
      let image = Image (attrs, float, src, title_opt) in
      match link_opt with
      | Some url -> Link (([], [image]), None, url)
      | None -> image in
    return r
  ) ||| (
  (* acronym *)
    p_until
      (p_pred (fun c -> c >= 'A' && c <= 'Z'))
      (p_char '(') >>= fun (acr, _) ->
    p_string_not_empty acr >>>
    p_str_until (p_char ')' >>> end_of_phrase) >>= fun desc ->
    (*p_str_until (closed_modifier (p_char ')')) >>= fun desc ->*)
    return (Acronym (acr, desc))
  )

(* ... and one with them. *)
and all_phrases end_of_phrase =
  (phrases_except_hyperlinks end_of_phrase) |||
  (* hyperlink *)
  (
    (* ...:http://komar.bitcheese.net *)
    let url =
      p_char ':' >>>
      p_until (p_not_whitespace) end_of_phrase >>= fun (url, _) -> return url in
    (* ...(title)'' *)
    let end_with_title =
      p_char '(' >>>
      (*p_str_until (check_prev p_not_whitespace >>> p_str ")\"") >>= fun title ->*)
      p_str_until (p_str ")\"") >>= fun title ->
      url >>= fun url ->
      return (title, url) in
    (* ...'' *)
    let end_with_no_title =
      (*check_prev p_not_whitespace >>>*)
      p_char '"' >>> url in

    p_char '"' >>>
    (* XXX: hm *)
    check_current p_not_whitespace >>>
    try_attrs (fun a ->
    current_pos >>= fun from ->
    collect
      ~what:phrases_except_hyperlinks
      ~ended_with:(end_of_phrase ||| dont_jump ((end_with_title >>> return ()) ||| (end_with_no_title >>> return ())))
      ~from
      ~until:(
        (end_with_title >>= fun (title, url) -> return (Some title, url)) |||
        (end_with_no_title >>= fun url -> return (None, url))
      ) >>= fun (line, (title_opt, url)) ->

    let r = Link ((a, line), title_opt, url) in
    return r)
  ) |||
  (* short hyperlink *)
  (
    p_char ':' >>>
    (* XXX: hm *)
    check_current p_not_whitespace >>>
    try_attrs (fun a ->
    p_str_until end_of_phrase >>= fun link ->
    (* FIXME *)
    p_string_not_empty link >>>
    let r = Link ((a, [CData link]), None, link) in
    return r)
  )

let line (s, pos) =
  (collect
    ~what:all_phrases
    ~ended_with:end_of_phrase
    ~from:pos
    ~until:p_end >>= fun (line, _) ->
  return line) (s, pos)

let line_of_string s =
  match line (s, 0) with
  | Parsed (r, _) -> r
  | Failed -> empty_line

let align =
  (p_str "<>" >>> return Justify) ||| (* must be first *)
  (p_char '<' >>> return Left)    |||
  (p_char '=' >>> return Center)  |||
  (p_char '>' >>> return Right)

let option =
  (attr_decl >>= fun x -> return (`Attr  x)) |||
  (align     >>= fun x -> return (`Align x)) |||
  (p_char '(' >>> return `Left_padding)  |||
  (p_char ')' >>> return `Right_padding)

(* should we fix it? *)
let add_option (attrs, talign, (lp, rp)) = function
  | `Attr a -> (a @ attrs, talign, (lp, rp))
    (* may be we need to add warning or something else
     * when align is already set *)
  | `Align a -> (attrs, Some a, (lp, rp))
  | `Left_padding -> (attrs, talign, (succ lp, rp))
  | `Right_padding -> (attrs, talign, (lp, succ rp))

let options =
  p_manyf option add_option default_options

let valign =
  (p_char '^' >>> return Top   ) |||
  (p_char '-' >>> return Middle) |||
  (p_char '~' >>> return Bottom)

let tableoption =
  (option >>= fun x -> return (`Option x)) |||
  (valign >>= fun x -> return (`Valign x))
let add_tableoption (opts, valign) = function
  | `Valign x -> (opts, Some x)
  | `Option x -> (add_option opts x, valign)
let tableoptions =
  p_manyf tableoption add_tableoption default_tableoptions
let tableoptions_plus =
  p_plusf tableoption add_tableoption default_tableoptions

let block_type =
  (p_char 'h' >>>
    p_pred (fun c -> c >= '1' && c <= '6') >>= fun c ->
    return (`Textblock (`Header (num_of_char c)))) |||
  (p_str "bq" >>> return (`Textblock `Blockquote)) |||
  (p_str "fn" >>> p_unsign_int >>= fun i ->
    return (`Textblock (`Footnote i))) |||
  (p_str "bc"  >>> return (`Textblock `Blockcode)) |||
  (p_str "pre" >>> return (`Textblock `Pre)) |||
  (* XXX: legacy code: must be removed *)
  (p_str "no" >>> (p_str "textile" ||| p_str "markup") >>>
    return (`Textblock `Blocknomarkup)) |||
  (p_char 'p'  >>> return (`Textblock `Paragraph)) |||
  (p_str "table" >>> return `Table) |||
  (p_str "toc" >>> return `ToC)

let block_modifier =
  p_many p_whitespace >>> (* skip whitespaces *)
  block_type >>= function
  | `Table ->
      tableoptions >>= fun topts ->
      p_opt () (p_char '.' >>> return ()) >>>
      p_many p_whitespace >>>
      p_end >>>
      return (`Table topts)
  | `ToC ->
      options >>= fun opts ->
      p_char '.' >>>
      p_many p_whitespace >>>
      p_end >>>
      return (`ToC opts)
  | `Textblock bm ->
      options >>= fun opts ->
      p_char '.' >>>
      ((p_char '.' >>> return true) ||| (return false)) >>= fun extended ->
      p_char ' ' >>>
      (* FIXME *)
      (*line >>= fun line ->*)
      (*dont_jump p_somechar >>>*)
      return (`Textblock (bm, opts, extended))

let rec next_block stream =

  (*let get_content parse_first parse empty is_ext =
    let rec loop acc (s, pos) =
      try
        let str = Stream.next stream in
        (
          (parse >>= fun r ->
          loop (r::acc)) |||
          (if is_ext
          then
            (match Stream.peek stream with
            | Some next_str ->
                (* wtf am i writing *)
                (fun _ ->
                  ((block_modifier >>> return (List.rev acc)) |||
                  (loop (empty::acc))) (next_str, 0))
            | None -> return (List.rev acc))
          else return (List.rev acc))
        ) (str, 0)
      with Stream.Failure -> (return (List.rev acc)) (s, pos) in
    parse_first >>= fun first ->
    loop [first] in*)

  let get_content parse_first parse empty extended (s, pos) =
    let rec loop acc =
      try
        let str = Stream.next stream in
        (match parse (str, 0) with
        | Parsed (r, _) -> loop (r::acc)
        | Failed when extended ->
            (match Stream.peek stream with
            | Some next_str ->
                (match (block_modifier (next_str, 0)) with
                | Parsed _ -> List.rev acc
                | Failed -> (loop (empty::acc)))
            | None -> List.rev acc)
        | Failed -> List.rev acc)
      with Stream.Failure -> List.rev acc in
    match parse_first (s, pos) with
    | Parsed (first, _) -> Parsed (loop [first], (s, pos))
    | Failed -> Failed in

  let get_lines extended (s, pos) =
    let parse_line       = line in
    let parse_first_line = line in
    get_content parse_first_line parse_line [] extended (s, pos) in

  let get_strings extended (s, pos) =
    let parse_string (s, pos) =
      match s with
      | ""  -> Failed
      | _ -> Parsed (s, (s, (String.length s))) in
    let parse_first_string (s, first) =
      let s = String.slice ~first s in
      parse_string (s, first) in
    get_content parse_first_string parse_string "" extended (s, pos) in

  let celloptions =
    let option =
      (p_char '_' >>> return `Head) |||
      (tableoption >>= fun x -> return (`Topt x)) |||
      (p_char '\\' >>> p_int >>= fun x -> return (`Colspan x)) |||
      (p_char '/'  >>> p_int >>= fun x -> return (`Rowspan x)) in
    let add (celltype, topts, ((colspan, rowspan) as cellspan)) = function
      | `Head -> (Head, topts, cellspan)
      | `Topt x -> (celltype, add_tableoption topts x, cellspan)
      | `Colspan x -> (celltype, topts, (Some x, rowspan))
      | `Rowspan x -> (celltype, topts, (colspan, Some x)) in
    p_plusf option add default_celloptions in

  let element c prev_level =
    let bullet = p_many p_whitespace >>> c in
    bullet >>>
    p_upto_timesf prev_level
      (p_many p_whitespace >>> c)
      (fun l _ -> succ l) 1 >>= fun lvl ->
    (* if you remove line below, strings started with Strong text will be
     * parsed as elements of list *)
    p_plus p_whitespace >>>
    line >>= fun line ->
    return (lvl, line) in

  let get_element c prev_level x =
    match Stream.peek stream with
    | Some s ->
        (element c prev_level >>= fun e ->
        return (Stream.junk stream; e)) (s, 0)
    | None -> Failed in

  let get_elements c =
    element (p_char c) 0 >>= fun ((f_e_lvl, _) as first_element) ->
    p_manyf_arg
      (fun (prev_lvl, elements) -> get_element (p_char c) prev_lvl)
      (fun (_, acc) (lvl, line) -> lvl, (lvl, line)::acc)
      (f_e_lvl, [first_element]) >>= fun (_, rev_elements) ->
    return (List.rev (rev_elements)) in

  let row peeks =
    (* FIXME: must be clean!!!1111 *)
    let peeks = ref peeks in
    (* suppose you has already parsed first '|' *)
    let get_cell =
      (* it's for |foo\nbar|
       * hate this *)
      let continue_cell x =
        let rec loop acc cell_peeks x =
          match peekn stream (!peeks + cell_peeks) with
          | None -> Failed
          | Some s ->
              (collect
                (* FIXME *)
                ~what:all_phrases
                ~ended_with:(end_of_phrase |||
                  (* FIXME *)
                  (* check if it works with |(@code@)| *)
                  dont_jump (
                    p_many p_punct >>>
                    p_char '|' >>> return ()))
                ~from:0
                ~until:(
                  (p_char '|' >>> return true) |||
                  (p_end >>> return false)
                ) >>= function
                | line, true ->
                    return (peeks := !peeks + (succ cell_peeks); List.rev (line::acc))
                | line, false ->
                    loop (line::acc) (succ cell_peeks)
              ) (s, 0) in
        loop [] 0 x in

      p_opt default_celloptions (
        celloptions >>= fun copts ->
        p_str ". " >>>
        return copts) >>= fun copts ->
      (
        (* empty cell *)
        (p_char '|' >>> return (empty_line, true)) |||
        (current_pos >>= fun beg_of_line ->
        collect
          ~what:all_phrases
          (* FIXME *)
          ~ended_with:(end_of_phrase |||
            (* FIXME *)
            dont_jump (p_many p_punct >>> p_char '|' >>> return ()))
          ~from:beg_of_line
          ~until:(
            (p_char '|' >>> return true) |||
            (p_end >>> return false)
          ))
      ) >>= function
      | first_line, true -> return (copts, [first_line])
      | first_line, false -> continue_cell >>= fun lines ->
      return (copts, first_line::lines) in

    p_many p_whitespace >>> (* skip whitespaces *)
    p_opt default_tableoptions (
      tableoptions_plus >>= fun topts ->
      p_char '.' >>>
      p_plus p_whitespace >>>
      return topts) >>= fun topts ->
    p_char '|' >>>
    get_cell >>= fun first_cell ->
    p_manyf_ends_with
      get_cell
      (fun acc x -> x :: acc)
      [first_cell]
      p_end >>= fun rev_cells ->
    return (njunk stream !peeks; (topts, List.rev rev_cells)) in

  let get_extra_rows =
    p_seq
      (fun _ ->
        match Stream.peek stream with
        | None -> Failed
        | Some s -> row 1 (s, 0)) in

  let get_rows =
    row 0 >>= fun first_row ->
    get_extra_rows >>= fun extra_rows ->
    return (first_row::extra_rows) in

  let get_block s =
    (
      (* block marked with modifier *)
      (block_modifier >>= function
        | `Textblock (bm, opts, extended) ->
            let lines   f = get_lines   extended >>= fun r -> return (f r) in
            let strings f = get_strings extended >>= fun r -> return (f r) in
            (match bm with
            | `Header lvl    -> lines   (fun x -> Header (lvl,  (opts, x)))
            | `Blockquote    -> lines   (fun x -> Blockquote    (opts, x))
            | `Footnote n    -> lines   (fun x -> Footnote (n,  (opts, x)))
            | `Blockcode     -> strings (fun x -> Blockcode     (opts, x))
            | `Pre           -> strings (fun x -> Pre           (opts, x))
            | `Blocknomarkup -> strings (fun x -> Blocknomarkup (opts, x))
            | `Paragraph     -> lines   (fun x -> Paragraph     (opts, x)))
        | `Table topts ->
            (get_extra_rows >>= function
            | [] -> fail
            | rows ->  return (Table (topts, rows)))
        | `ToC opts ->
            (match Stream.peek stream with
            | None | Some "" (* FIXME *) ->
                Stream.junk stream; return (ToC opts)
            | _ -> fail)
      (* only table *)
      ) ||| (
        get_rows >>= fun rows ->
        return (Table (default_tableoptions, rows))
      (* bullist *)
      ) ||| (
        get_elements '*' >>= fun el -> return (Bulllist el)
      (* numlist *)
      ) ||| (
        get_elements '#' >>= fun el -> return (Numlist  el)
      (* usual text paragraph *)
      ) ||| (
        get_lines false >>= fun lines ->
        return (Paragraph (default_options, lines))
      )
    ) (s, 0) >> function
    | Parsed (r, _) -> r
    | Failed -> assert false (* FIXME *) in

  try
    match Stream.next stream with
    |  ""  -> next_block stream
    | fstr -> Some (get_block fstr)
  with Stream.Failure -> None

let of_stream stream =
  Stream.from (fun _ -> next_block stream)

let enum stream =
  Enum.from (fun () ->
    match next_block stream with
    | Some b -> b
    | None -> raise Enum.No_more_elements)

