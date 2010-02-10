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

(** Textile markup language support for OCaml

  @see
  <http://thresholdstate.com/articles/4312/the-textile-reference-manual>
  the official textile reference manual
  @author Alexander Markov 2009 apsheronets\@gmail.com
*)

(** {2 Textile syntax tree} *)

(** {3 Options} *)

type attr =
  | Class    of string (** p(myclass). *)
  | Id       of string (** p(#myid). *)
  | Style    of string (** p\{color:red\}. *)
  | Language of string (** p\[fr-fr\]. *) (* without backslashes :) *)

(** Alignment option. *)
type talign =
  | Right   (** > *)
  | Left    (** < *)
  | Center  (** = *)
  | Justify (** <> *)

type valign =
  | Top    (** ^ *)
  | Middle (** - *)
  | Bottom (** ~ *)

(** Left and right padding consistently. Define with ( and ) in block modifier *)
type padding =
  int * int

type options =
  attr list * talign option * padding

(** {3 Content} *)

(** Phrases may be presents like HTML tags for text formatting. For
example, **ocaml is __functional__ language** is equivalent for <b>ocaml
is <i>functional</i> language</b> or [Bold [CData "ocaml is "; Italic
[CData "functional"]; CData " language"]] *)
type phrase =
  | CData       of string
  | Emphasis    of (attr list * phrase list)   (** _ *)
  | Strong      of (attr list * phrase list)   (** * *)
  | Italic      of (attr list * phrase list)   (** __ *)
  | Bold        of (attr list * phrase list)   (** ** *)
  | Citation    of (attr list * phrase list)   (** ?? *)
  | Deleted     of (attr list * phrase list)   (** - *)
  | Inserted    of (attr list * phrase list)   (** + *)
  | Superscript of (attr list * phrase list)   (** ^ *)
  | Subscript   of (attr list * phrase list)   (** ~ *)
  | Span        of (attr list * phrase list)   (** % *)
  | Code        of (attr list * phrase list)   (** @ *)
  | Acronym of string * string               (** ABC(Always Be Closing *)
  | Image of attr list * string * string option (** !/fear.jpg(my wife)! *)
  | Link of (attr list * phrase list) *
      string option * string (** "linktext(title)":url *)

(** One line of text. It terminates by line break character. *)
type line =
  phrase list

(** {3 Tables} *)

type cellspan =
  int option * int option

(** Table specific options. May be applied to a table, to a row or to a cell *)
type tableoptions =
  options * valign option

type celltype =
  | Data
  | Head

type cell =
  (celltype * tableoptions * cellspan) * line list

type row =
  tableoptions * cell list

type element =
  int * line

(** {2 Blocks} *)

(** Extended blocks parse automaticly so there is no difference for you between normal and extended blocks.

You have to escape symbols in Blockcode and Pre by yourself because I don't know in what format will you convert the AST! *)
type block =
  | Header     of int * (options * line list) (** h1. *)
  | Blockquote of (options * line list)       (** bq. *)
  | Footnote   of int * (options * line list) (** fnn. *)
  | Paragraph  of (options * line list)     (** p. *)
  | Blockcode  of (options * string list)   (** bc. *)
  | Pre        of (options * string list)   (** pre. *)
  | Numlist    of (options * element list)  (** # *)
  | Bulllist   of (options * element list)  (** * *)
  | Table      of (tableoptions * row list) (** |t|a|b|l|e| *)

(** {2 Functions} *)

val parse_stream : string Stream.t -> block Stream.t
