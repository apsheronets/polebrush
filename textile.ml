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
  | Link of string * phrase list (* "linktext":url *)
type line =
  phrase list
type align =
  | Right   (* > *)
  | Left    (* < *)
  | Center  (* = *)
  | Justify (* <> *)
type options =
  attr list * align option
type block =
  | Header     of int * (options * line list) (** h1. *)
  | Blockquote of (options * line list)       (** bq. *)
  | Footnote   of int * (options * line list) (** fnn. *)
  | Paragraph  of (options * line list)   (** p. *)
  | Blockcode  of (options * string list) (** bc. *)
  | Pre        of (options * string list) (** pre. *)
  | Numlist    of (options * line list)   (** # *)
  | Bulllist   of (options * line list)   (** * *)
  (*| Table of FIXME *)


exception Invalid_block_modifier

let num_of_char c =
  (int_of_char c) - 48

type block_modifier =
  | BHeader of int
  | BBlockquote
  | BFootnote of int
  | BParagraph
  | BBlockcode
  | BPre
  | BNumlist
  | BBulllist

let parse_stream stream =

  let rec parse_string str =
    (* Cut empty phrases *)
    if String.length str = 0 then [] else
    let pack_cdata str start len =
      CData (String.sub str start len) in
    let rec find_modifier prev_char n =
      try
        match prev_char, str.[n] with
        | ' ', '_' ->
            (match str.[n+1] with
            | '_' ->
                close_modifier (n+2) n ['_'; '_'] (fun x -> Italic x)
            | _ ->
                close_modifier (n+1) n ['_'] (fun x -> Emphasis x))
        | ' ', '*' ->
            (match str.[n+1] with
            | '*' ->
                close_modifier (n+2) n ['*'; '*'] (fun x -> Bold x)
            | _ ->
                close_modifier (n+1) n ['*'] (fun x -> Strong x))
        | ' ', '?' ->
            (match str.[n+1] with
            | '?' ->
                close_modifier (n+2) n ['?'; '?'] (fun x -> Citation x)
            | _ -> find_modifier '?' (n+1)
            )
        | ' ', '-' ->
            close_modifier (n+1) n ['-'] (fun x -> Deleted x)
        | ' ', '+' ->
            close_modifier (n+1) n ['+'] (fun x -> Inserted x)
        | ' ', '^' -> (* FIXME: is it a good implementation? *)
            close_modifier (n+1) n ['^'] (fun x -> Superscript x)
        | ' ', '~' ->
            close_modifier (n+1) n ['~'] (fun x -> Subscript x)
        | ' ', '%' ->
            close_modifier (n+1) n ['%'] (fun x -> Span x)
        | ' ', '@' ->
            close_modifier (n+1) n ['@'] (fun x -> Code x)
        | _, c -> find_modifier c (n+1)
      (* If we have passed whole string without any modifier
       * then we simply pack it in CData *)
      with Invalid_argument _ -> [CData str]
                          (* End of last lexeme position
                           * vvvv *)
    and close_modifier start eoll char_list constr =
      (* FIXME: jumping *)
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
        with Invalid_argument _ -> (*CData str (* FIXME *)*)
          match char_list with
          (* This branch ... *) (* Goddamn, FOR WHAT did I create *)
          (*| [] -> *)          (* this branch? *)
          (* FAIL *)
          | _  -> find_modifier str.[start-1] start in
      loop char_list start in
    find_modifier ' ' 0 in

  let get_options fstr start =
    let rec loop attrs align n =
      try
        match fstr.[n], align with
        | '{', _ ->
            extr_attr_and_cont n '}' (fun x -> Style x) attrs align
        | '(', _ ->
            (match fstr.[n+1] with
            | '#' ->
              extr_attr_and_cont (n+1) ')' (fun x -> Id x) attrs align
            |  _  ->
              extr_attr_and_cont n ')' (fun x -> Class x) attrs align)
        | '[', _ ->
            extr_attr_and_cont n ']' (fun x -> Language x) attrs align
        | '<', None -> (match fstr.[n+1] with
            | '>' -> loop attrs (Some Justify) (n+2)
            |  _  -> loop attrs (Some Left) (n+1))
        | '>', None -> loop attrs (Some Right) (n+1)
        | '=', None -> loop attrs (Some Justify) (n+1)
        | '.', _ -> (match fstr.[n+1] with
            | ' ' -> false, (attrs, align), (n+2)
            | '.' -> (match fstr.[n+2] with
                | ' ' -> true, (attrs, align), (n+2)
                |  _  -> raise Invalid_block_modifier)
            |  _  -> raise Invalid_block_modifier) (* whitespace required *)
        |  _ -> raise Invalid_block_modifier
      (* If we have passed the whole string and haven't found a dot *)
      with Invalid_argument _ ->
        raise Invalid_block_modifier
    (* Extracts an attribute which closes by char c *)
    and extr_attr_and_cont n c constr attrs align =
      (try
        let e = String.index_from fstr (n+1) c in
        let result = constr (String.sub fstr (n+1) (e-n-1)) in
        loop (result :: attrs) align (e+1)
      with
       (* If we have an open parenthesis and some happened shit
        * then we stop to parse and leave string "s" as is *)
        Not_found | Invalid_argument _ -> raise Invalid_block_modifier)
    in loop [] None start in

  let get_block_modifier fstr =
    let options =
      get_options fstr in
    try
      Some (match fstr.[0], fstr.[1], fstr.[2] with
        (* Headers  *)
        | 'h', c,  _
          when (c >= '0') && (c <= '6') ->
            BHeader (num_of_char c), options 2
        | 'b','q', _  ->
            BBlockquote, options 2
        | 'f','n', c  ->
            (* It just works
             * I don't know how *)
            let check x = (x >= 0) && (x <= 9) in
            let rec loop acc n =
              let num = num_of_char fstr.[n] in
              if check num
              then loop ((acc*10)+num) (n+1)
              else
                BFootnote acc, options n in
            let num = num_of_char fstr.[2] in
            if check num
            then loop num 3
            else raise Invalid_block_modifier
        | 'b','c', _  ->
            BBlockcode, options 2
        | 'p','r','e' ->
            BPre, options 3
        | 'p', _,  _  ->
            BParagraph, options 1
        | _ -> raise Invalid_block_modifier)
    with
      (* If our string is too shorter... *)
      | Invalid_argument _
      | Invalid_block_modifier -> None in

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
    | Some (block_modifier, (is_ext, options, start)) ->
        let get_lines () = get_func parse_string fstr is_ext start in
        let get_strings () = get_func (fun x -> x) fstr is_ext start in
        (match block_modifier with
        | BHeader n   -> Header   (n, (options, get_lines ()))
        | BBlockquote -> Blockquote   (options, get_lines ())
        | BFootnote n -> Footnote (n, (options, get_lines ()))
        | BParagraph  -> Paragraph    (options, get_lines ())
        | BBlockcode  -> Blockcode    (options, get_strings ())
        | BPre        -> Pre          (options, get_strings ())
        | BNumlist    -> Numlist      (options, get_lines ())
        | BBulllist   -> Bulllist     (options, get_lines ()))
    | None ->
        Paragraph (([], None), get_func parse_string fstr false 0) in

  let next_block () =
    try
      let fstr = Stream.next stream in
      Some (get_block fstr)
    with Stream.Failure -> None in

  Stream.from (fun _ -> next_block ())
