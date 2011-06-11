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

(** Textile markup language syntax tree

  @see
  <http://thresholdstate.com/articles/4312/the-textile-reference-manual>
  the official textile reference manual
  @author Alexander Markov apsheronets\@gmail.com
*)

(** {2 Textile syntax tree} *)

(** {3 Options} *)

type attr =
  | Class    of string (** p(myclass). *)
  | Id       of string (** p(#myid). *)
  | Style    of string (** p\{color:red\}. *)
  | Language of string (** p\[fr-fr\]. *) (* without backslashes :) *)

(** Image float. *)
type img_float =
  | Float_left  (** < *)
  | Float_right (** > *)

(** Text-alignment option. *)
type talign =
  | Right   (** > *)
  | Left    (** < *)
  | Center  (** = *)
  | Justify (** <> *)

(** Vertical alignment. *)
type valign =
  | Top    (** ^ *)
  | Middle (** - *)
  | Bottom (** ~ *)

(** Left and right padding consistently. Define with ( and ) in block
modifier. (0,0) if padding doesn't set. *)
type padding =
  int * int

type options =
  attr list * talign option * padding

(** {3 Content} *)

(** Phrases may be presents like HTML tags for text formatting. For
example, **ocaml is __functional__ language** is equivalent for <b>ocaml
is <i>functional</i> language</b> or [Bold ([], [CData "ocaml is ";
Italic ([], [CData "functional"]); CData " language"])] *)

type phrase =
  | CData       of string
  | Emphasis    of (attr list * phrase list) (** _ *)
  | Strong      of (attr list * phrase list) (** * *)
  | Italic      of (attr list * phrase list) (** __ *)
  | Bold        of (attr list * phrase list) (** ** *)
  | Citation    of (attr list * phrase list) (** ?? *)
  | Deleted     of (attr list * phrase list) (** - *)
  | Inserted    of (attr list * phrase list) (** + *)
  | Superscript of (attr list * phrase list) (** ^ *)
  | Subscript   of (attr list * phrase list) (** ~ *)
  | Span        of (attr list * phrase list) (** % *)
  | Code        of (attr list * string)      (** @ *)
  | Notextile   of string                    (** == *)
  | Acronym of string * string               (** ABC(Always Be Closing) *)
  | Image of attr list * img_float option *
      string * string option (** !imgsrc(alt)! *)
  | Link of (attr list * phrase list) *
      string option * string (** "linktext(title)":url *)
  | Reference of int         (** \[1\] *)

(** One line of text. It terminates by line break character. *)
type line =
  phrase list

(** One element of a list. It's a line and depth of element,
or just count of asterisk or sharps. *)
type element =
  int * line

(** {3 Tables} *)

(** Table specific options. May be applied to a table or to a row. *)
type tableoptions =
  options * valign option

(** In textile symbol _ defines a cell as a table header. Otherwise
it's a regular data cell. *)
type celltype =
  | Data (** | <...> | *)
  | Head (** |_. <...> | *)

(** Colspan and rowspan. *)
type cellspan =
  int option * int option

(** Cell specific options. *)
type celloptions =
  celltype * tableoptions * cellspan

(** A cell in row. *)
type cell =
  celloptions * line list

(** A row in table. *)
type row =
  tableoptions * cell list

(** {2 Blocks} *)

(** Extended blocks parse automaticly so there is no difference for you between normal and extended blocks. *)
type block =
  | Header     of int * (options * line list) (** h1. *)
  | Blockquote of (options * line list)       (** bq. *)
  | Footnote   of int * (options * line list) (** fnn. *)
  | Paragraph  of (options * line list)     (** p. *)
  | Blockcode  of (options * string list)   (** bc. *)
  | Pre        of (options * string list)   (** pre. *)
  | Blocknott  of (options * string list)   (** notextile. *)
  | Numlist    of element list              (** # *)
  | Bulllist   of element list              (** * *)
  | Table      of (tableoptions * row list) (** |t|a|b| *)

(** {2 Translation} *)

(** Translates the line to a simple string which can be used, for example,
in HTML's <title> tag. All markup will be removed. *)
val string_of_line: line -> string
