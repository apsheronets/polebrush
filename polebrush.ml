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
open Printf

type attr =
  | Class    of string (* p(myclass). *)
  | Id       of string (* p(#myid). *)
  | Style    of string (* p{color:red}. *)
  | Language of string (* p[fr-fr]. *)
type img_float =
  | Float_left
  | Float_right
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
  | Code        of (attr list * string)      (* @ *)
  | Nomarkup    of string                    (* == *)
  | Acronym of string * string               (* ABC(Always Be Closing *)
  | Image of attr list * img_float option
      * string * string option (* !imgsrc(alt)! *)
  | Link of (attr list * phrase list) *
      string option * string (* "linktext(title)":url *)
  | Reference of int (* [1] *)
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
  celltype * tableoptions * cellspan
type cell =
  celloptions * line list
type row =
  tableoptions * cell list
type element =
  int * line
type block =
  | Header        of int * (options * line list) (* h1. *)
  | Abstract      of (options * line list)       (* abstract. *)
  | Blockquote    of (options * line list)       (* bq. *)
  | Footnote      of int * (options * line list) (* fnn. *)
  | Paragraph     of (options * line list)     (* p. *)
  | Blockcode     of (options * string list)   (* bc. *)
  | Pre           of (options * string list)   (* pre. *)
  | Blocknomarkup of (options * string list)   (* nomarkup. *)
  | Numlist       of element list              (* # *)
  | Bulllist      of element list              (* * *)
  | Table         of (tableoptions * row list) (* |t|a|b| *)
  | ToC           of options                   (* toc. *)


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
        | Span        (_, l) ->
            add (string_of_line l)
        | Acronym (s, _)
        | Code    (_, s) ->
            add s
        | Nomarkup _ (* TODO: whoops, what we have to do? *)
        | Image _ -> ()
        | Link ((_, l), _, _) ->
            add (string_of_line l)
        | Reference i ->
            Printf.bprintf buf "[%d]" i);
        loop t
    | [] -> Buffer.contents buf in
  loop line
