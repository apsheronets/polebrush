(** Textile markup language support for OCaml

  @see
  <http://thresholdstate.com/articles/4312/the-textile-reference-manual>
  the official textile reference manual
  @author Alexander Markov (2009-2010) apsheronets\@gmail.com
*)

(** {9 Textile syntax tree} *)

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
  | Header     of int * (attr list * align option * line list) (* h1. *)
  | Blockquote of (attr list * align option * line list) (* bq. *)
  | Footnote   of int * (attr list * align option * line list) (* fnn. *) (* FIXME *)
  | Paragraph  of (attr list * align option * line list) (* p. *)
  | Blockcode  of (attr list * align option * line list) (* bc. *)
  | Pre        of (attr list * align option * line list) (* pre. *)
  | Numlist    of line list (* # *)
  | Bulllist   of line list (* * *)
  (*| Table of FIXME *)


val parse_stream : string Stream.t -> block Stream.t
