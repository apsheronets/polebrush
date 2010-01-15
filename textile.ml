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
 * Copyright 2009 Alexander Markov *)

open ExtLib

type attr =
  | Class    of string (* p(myclass). *)
  | Id       of string (* p(#myid). *)
  | Style    of string (* p{color:red}. *)
  | Language of string (* p[fr-fr]. *)
type phrase =
  | CData       of string
  | Emphasis    of phrase list   (* _ *)
  | Strong      of phrase list   (* * *)
  | Italic      of phrase list   (* __ *)
  | Bold        of phrase list   (* ** *)
  | Citation    of phrase list   (* ?? *)
  | Deleted     of phrase list   (* - *)
  | Inserted    of phrase list   (* + *)
  | Superscript of phrase list   (* ^ *)
  | Subscript   of phrase list   (* ~ *)
  | Span        of phrase list   (* % *)
  | Code        of phrase list   (* @ *)
  | Acronym of string * string   (* ABC(Always Be Closing *)
  | Link of string * string      (* "linktext":url *)
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
  attr list * talign option * valign option
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

(* This is internal exceptions. They must be even catched
 * inside the module *)
exception Invalid_modifier
exception Invalid_attribute
exception Invalid_table
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
  | TableWithAttrs of (celltype * tableoptions * int)
  | TableWithoutAttrs of row
type params_set =
  | TableParams
  | RowParams
  | CellParams
  | BlockParams
  | PhraseParams
type encasing_char =
  | Blank
  | Brace

let num_of_char c =
  (int_of_char c) - 48

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

  let rec parse_string str =
    (* Cut empty phrases *)
    if String.length str = 0 then [] else
    let pack_cdata str start len =
      CData (String.sub str start len) in
    let rec find_modifier prev_char n =
      try
        if prev_char = ' '
        then match str.[n], str.[n+1] with
        | '_', '_' -> cm (n+2) n ['_'; '_'] (fun x -> Italic x)
        | '_',  _  -> cm (n+1) n ['_']      (fun x -> Emphasis x)
        | '*', '*' -> cm (n+2) n ['*'; '*'] (fun x -> Bold x)
        | '*',  _  -> cm (n+1) n ['*']      (fun x -> Strong x)
        | '?', '?' -> cm (n+2) n ['?'; '?'] (fun x -> Citation x)
        | '-',  _  -> cm (n+1) n ['-']      (fun x -> Deleted x)
        | '+',  _  -> cm (n+1) n ['+']      (fun x -> Inserted x)
        | '^',  _  -> cm (n+1) n ['^']      (fun x -> Superscript x)
        | '~',  _  -> cm (n+1) n ['~']      (fun x -> Subscript x)
        | '%',  _  -> cm (n+1) n ['%']      (fun x -> Span x)
        | '@',  _  -> cm (n+1) n ['@']      (fun x -> Code x)
        | _ -> find_modifier str.[n] (n+1)
        else find_modifier str.[n] (n+1)
      (* If we have passed whole string without any modifier
       * then we simply pack it in CData *)
      with Invalid_argument _ -> [CData str]
              (* End of last lexeme position
               * vvvv *)
    and cm start eoll char_list constr =
      if str.[start] = ' ' then find_modifier ' ' (start+1) else
      (* don't forget what chlist is not the same as char_list! *)
      let rec loop clist n =
        try
          match str.[n], clist with
          | c, [h]  when c = h ->
              (* PLEASE FIXME *
               * PLEEEEEAAASE *)
              let tail =
                constr (parse_string (
                  String.sub str start (n-start-(List.length char_list - 1))
                ))
                :: parse_string (
                  let s = n + (List.length clist) in
                  String.sub str s ((String.length str) - s)
                ) in
              (* Fixes empty strings in lines like ^"_some line_"$ *)
              if eoll = 0 then tail
              else pack_cdata str 0 eoll :: tail
          | c, h::t when c = h -> loop t (n+1)
          | _ -> loop clist (n+1)
        with Invalid_argument _ -> find_modifier str.[start-1] start in
      loop char_list start in
    find_modifier ' ' 0 in

  let get_options is_table fstr start = (* FIXME, it's too ugly *)
    let rec loop ((attrs, talign, ((leftpad, rightpad) as padding),
      valign, celltype, cellspan) as options) n =
      try
        match is_table, fstr.[n], talign, valign, celltype with

        (* Style *)
        | _, '{', _, _, _ ->
            extr_attr_and_cont n '}' (fun x -> Style x) options

        (* This may be a class, an id or left padding *)
        | _, '(', _, _, _ ->
            (try
              match fstr.[n+1] with
              | '#' ->
                extr_attr_and_cont (n+1) ')' (fun x -> Id x) options
              |  _  ->
                extr_attr_and_cont n ')' (fun x -> Class x) options
            with
              (* If it's not an attribute
               * then try to parse as left alignment *)
              | Invalid_attribute ->
                  if is_table (* But only if it's not a table *)
                  then raise Invalid_modifier
                  else loop (attrs, talign, (leftpad+1, rightpad), valign,
                    celltype, cellspan) (n+1))

        (* Language *)
        | _, '[', _, _, _ ->
            extr_attr_and_cont n ']' (fun x -> Language x) options

        (* Right padding *)
        | false, ')', _, _, _ ->
            loop (attrs, talign, (leftpad, rightpad+1), valign, celltype,
              cellspan) (n+1)

        (* Text alignment *)
        | _, '<', None, _, _ -> (match fstr.[n+1] with
            | '>' -> loop (attrs, (Some Justify), padding, valign,
                       celltype, cellspan) (n+2)
            |  _  -> loop (attrs, (Some Left), padding, valign,
                       celltype, cellspan) (n+1))
        | _, '>', None, _, _ -> loop (attrs, (Some Right), padding,
                                  valign, celltype, cellspan) (n+1)
        | _, '=', None, _, _ -> loop (attrs, (Some Justify), padding,
                                  valign, celltype, cellspan) (n+1)

        (* Heading cell *)
        | true, '_', _, _, Data -> loop (attrs, talign, padding, valign,
                                     Head, cellspan) (n+2)

        (* Vertical alignment *)
        | true, '^', _, None, _ -> loop (attrs, talign, padding,
                                     (Some Top), celltype, cellspan) (n+1)
        | true, '-', _, None, _ -> loop (attrs, talign, padding,
                                     (Some Middle),celltype,cellspan) (n+1)
        | true, '~', _, None, _ -> loop (attrs, talign, padding,
                                     (Some Bottom),celltype,cellspan) (n+1)

        (*| true, '\', _, _, _ ->*)

        (* End of options *)
        | _, '.', _, _, _ ->
            (try
              (match is_table, fstr.[n+1] with
              |   _,   ' ' -> false, options, (n+2)
              | false, '.' ->
                  (match fstr.[n+2] with
                  | ' ' -> true, options, (n+2)
                  |  _  -> raise Invalid_modifier)
              |  _  -> raise Invalid_modifier) (* whitespace required *)
            with Invalid_argument _ ->
              if is_table
              then false, options, (n+2)
              else raise Invalid_modifier)

        |  _ -> raise Invalid_modifier
      (* If we have passed the whole string and haven't found a dot *)
      with Invalid_argument _ | Invalid_attribute ->
        raise Invalid_modifier
    (* Extracts an attribute which closes by char c *)
    and extr_attr_and_cont n c constr (attrs, ta, pd, va, ct, cs) =
      (try
        let e = String.index_from fstr (n+1) c in
        let result = constr (String.sub fstr (n+1) (e-n-1)) in
        loop ((result :: attrs), ta, pd, va, ct, cs) (e+1)
      with
       (* If we have an open parenthesis and some happened shit *)
        Not_found | Invalid_argument _ -> raise Invalid_attribute)
    in loop ([], None, (0,0), None, Data, (None, None)) start in

  let rowoptions = ([], None, None) in
  let celloptions = (Data, ([], None, None), (None, None)) in

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

  let get_row str peeks =
    let rec loop str acc peeks start =
      (match get_celllines str peeks start with
      | Some (celllines, str, peeks, start) ->
          loop str ((celloptions, celllines)::acc) peeks start
      | None ->
          njunk stream peeks;
          List.rev acc) in
    loop str [] peeks 1 in


  let get_rows fstr frow =
    let rec loop acc str peeks =
      try
        let row = (rowoptions, get_row str peeks) in
        (match Stream.peek stream with
        | Some nextstr ->
            if String.length nextstr > 0
            then
              (match nextstr.[0] with
              | '|' ->
                  loop (row::acc) nextstr 1
              |  _  -> List.rev acc)
            else List.rev (row::acc)
        | None -> List.rev acc)
      with Invalid_row -> List.rev acc in
    loop [frow] fstr 0 in

  let get_block_modifier fstr =
    let options start =
      let is_ext, (attrs, align, padding, _, _, _), start
        = get_options false fstr start in
      (attrs, align, padding), (is_ext, start) in
    let tableoptions start =
      let _, (attrs, talign, _, valign, celltype, cellspan), start
        = get_options true fstr start in
      celltype, (attrs, talign, valign), start in
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
        | 't','a','b' -> (match fstr.[3], fstr.[4] with
            | 'l', 'e' -> TableWithAttrs (tableoptions 5)
            | _ -> raise Invalid_modifier)
        | '|', _,  _  ->
            (try
              TableWithoutAttrs (rowoptions, get_row fstr 0)
            with Invalid_row -> raise Invalid_modifier)
        | _ -> raise Invalid_modifier)
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
        | TableWithAttrs (ct, t, s) -> raise Invalid_modifier
        | TableWithoutAttrs frow ->
            Table (([], None, None), get_rows fstr frow))
    | None ->
        Paragraph (([], None, (0,0)), get_func parse_string fstr false 0) in

  let rec next_block () =
    try
      match Stream.next stream with
      |  ""  -> next_block ()
      | fstr -> Some (get_block fstr)
    with Stream.Failure -> None in

  Stream.from (fun _ -> next_block ())
