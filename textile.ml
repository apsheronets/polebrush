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
  | Emphasis    of phrase list (* _ *)
  | Strong      of phrase list (* * *)
  | Italic      of phrase list (* __ *)
  | Bold        of phrase list (* ** *)
  | Citation    of phrase list (* ?? *)
  | Deleted     of phrase list (* - *)
  | Inserted    of phrase list (* + *)
  | Superscript of phrase list (* ^ *)
  | Subscript   of phrase list (* ~ *)
  | Span        of phrase list (* % *)
  | Code        of phrase list (* @ *)
  | Acronym of string * string (* ABC(Always Be Closing *)
  | Link of string * phrase    (* "linktext":url *)
type line =
  phrase list
type align =
  | Right   (* > *)
  | Left    (* < *)
  | Center  (* = *)
  | Justify (* <> *)
type block =
  | Header     of int * (attr list * align option * line list) (** h1. *)
  | Blockquote of (attr list * align option * line list)       (** bq. *)
  | Footnote   of int * (attr list * align option * line list) (** fnn. *)
  | Paragraph  of (attr list * align option * line list) (** p. *)
  | Blockcode  of (attr list * align option * line list) (** bc. *)
  | Pre        of (attr list * align option * line list) (** pre. *)
  | Numlist    of (attr list * align option * line list) (** # *)
  | Bulllist   of (attr list * align option * line list) (** * *)
  (*| Table of FIXME *)

exception Parse_failure

let num_of_char c =
  (int_of_char c) - 48

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
  let line_of_string str =
    [CData str] in
  let get_lines (f:string) (start:int) parsing_func =
    let rec loop acc =
      try
        let str = Stream.next stream in
        match str with
        | ""  -> List.rev acc
        | str ->
            let result = parsing_func str in
            loop (result::acc)
      with Stream.Failure -> List.rev acc in
    let first_line =
      parsing_func (String.sub f start ((String.length f) - start)) in
    loop [first_line] in
  let get_attrs_and_align f start =
    let rec loop attrs align n =
      try
        match f.[n], align with
        | '{', _ ->
            extract_attr_and_continue n '}' (fun x -> Style x) attrs align
        | '(', _ ->
            (* FIXME: doesn't support ids *)
            extract_attr_and_continue n ')' (fun x -> Class x) attrs align
        | '[', _ ->
            extract_attr_and_continue n ']' (fun x -> Language x) attrs align
        | '<', None -> (match f.[n+1] with
            | '>' -> loop attrs (Some Justify) (n+2)
            |  _  -> loop attrs (Some Left) (n+1))
        | '>', None -> loop attrs (Some Right) (n+1)
        | '=', None -> loop attrs (Some Justify) (n+1)
        | '.', _ -> (match f.[n+1] with
            | ' ' -> (attrs, align), (n+2)
            |  _  -> raise Parse_failure)
        |  _ -> raise Parse_failure
      (* If we have passed the whole string and haven't found a dot *)
      with Invalid_argument _ ->
        raise Parse_failure
    (* Extracts an attribute which closes by char c *)
    and extract_attr_and_continue n c constr attrs align =
      (try
        let e = String.index_from f (n+1) c in
        let result = constr (String.sub f (n+1) (e-n-1)) in
        loop (result :: attrs) align (e+1)
      with
       (* If we have an open parenthesis and some happened shit
        * then we stop to parse and leave string "s" as is *)
        Not_found | Invalid_argument _ -> raise Parse_failure)
    in loop [] None start in
  (* This function returns None if there are no valid block modifier here
   * or Some turple which contains:
     * function which returns a block,
     * starting position to parse the first string of block (excluding
       modifier),
     * function which returns a line *)
  let get_block_modifier f =
    try
      match f.[0], f.[1], f.[2] with
        (* Headers  *)
        | 'h', c,  _
          when (let n = num_of_char c in
          (n >= 0) && (n <= 6)) ->
            Some ((fun x -> Header ((num_of_char c), x)), 2, parse_string)
        | 'b','q', _  ->
            Some ((fun x -> Blockquote x), 2, parse_string)
        | 'f','n', c  ->
            let check x = (x >= 0) && (x <= 9) in
            let rec loop acc n =
              let num = num_of_char f.[n] in
              if check num
              then loop ((acc*10)+num) (n+1)
              else
                Some ((fun x -> Footnote (acc, x)), n, parse_string) in
            let num = num_of_char f.[2] in
            if check num
            then loop num 3
            else None
        | 'b','c', _  ->
            Some ((fun x -> Blockcode x), 2, line_of_string)
        | 'p','r','e' ->
            Some ((fun x -> Pre x), 2, parse_string)
        | 'p', _,  _  ->
            Some ((fun x -> Paragraph x), 1, parse_string)
        | _ -> None
    with
      (* If our string is too shorter... *)
      | Invalid_argument _ -> None in
  let get_block_constr f =
    match get_block_modifier f with
    | Some (block_modifier, i, parsing_func) ->
        (try
          let (attrs, align), start = get_attrs_and_align f i in
          (fun x -> block_modifier (attrs, align, x)), start, parsing_func
        with Parse_failure ->
          (fun x -> Paragraph ([], None, x)), 0, parse_string)
    | None -> (fun x -> Paragraph ([], None, x)), 0, parse_string in
  let get_block f =
    let constr, start, parsing_func = get_block_constr f in
    (constr (get_lines f start parsing_func)) in
  let rec next_block () =
    try
      let first_string = Stream.next stream in
      match first_string with
      | "" -> next_block () (* empty line in the beginning of block *)
      | f  -> Some (get_block f)
    with Stream.Failure -> None in
  Stream.from (fun _ -> next_block ())

