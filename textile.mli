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
  | Emphasis    of phrase (* _ *)
  | Strong      of phrase (* * *)
  | Italic      of phrase (* __ *)
  | Bold        of phrase (* ** *)
  | Citation    of phrase (* ?? *)
  | Deleted     of phrase (* - *)
  | Inserted    of phrase (* + *)
  | Superscript of phrase (* ^ *)
  | Subscript   of phrase (* ~ *)
  | Span        of phrase (* % *)
  | Code        of phrase (* @ *)
  | Link of string * phrase
type line =
  phrase list
type align =
  | Right   (* > *)
  | Left    (* < *)
  | Center  (* = *)
  | Justify (* <> *)
type element =
  | Phrase  of phrase
  | Element of element
type block =
  | Header1    of (attr list * align option * line list) (* h1. *)
  | Header2    of (attr list * align option * line list) (* h2. *)
  | Header3    of (attr list * align option * line list) (* h3. *)
  | Blockquote of (attr list * align option * line list) (* bq. *)
  | Footnote   of (attr list * align option * line list) (* fnn. *) (* FIXME *)
  | Paragraph  of (attr list * align option * line list) (* p. *)
  | Blockcode  of (attr list * align option * line list) (* bc. *)
  | Pre        of (attr list * align option * line list) (* pre. *)
  | Numlist    of element list (* # *)
  | Bulllist   of element list (* * *)
  (*| Table of FIXME *)

val teststream: string Stream.t

val parse_stream : string Stream.t -> block Stream.t
