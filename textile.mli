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
  | Emphasis    of phrase list   (** _ *)
  | Strong      of phrase list   (** * *)
  | Italic      of phrase list   (** __ *)
  | Bold        of phrase list   (** ** *)
  | Citation    of phrase list   (** ?? *)
  | Deleted     of phrase list   (** - *)
  | Inserted    of phrase list   (** + *)
  | Superscript of phrase list   (** ^ *)
  | Subscript   of phrase list   (** ~ *)
  | Span        of phrase list   (** % *)
  | Code        of phrase list   (** @ *)
  | Acronym of string * string   (** ABC(Always Be Closing) *)
  | Image of string * string     (** !/fear.jpg(my wife)! *)
  | Link of phrase list * string (** "linktext":url *)

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
  | Numlist    of (options * line list)     (** # *)
  | Bulllist   of (options * line list)     (** * *)
  | Table      of (tableoptions * row list) (** |t|a|b|l|e| *)

(** {2 Functions} *)

val parse_stream : string Stream.t -> block Stream.t
