type attr =
  | Class    of string (* p(myclass). *)
  | Id       of string (* p(#myid). *)
  | Style    of string (* p{color:red}. *)
  | Language of string (* p[fr-fr]. *)
type phrase =
  | CData       of string list
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
type align =
  | Right   (* > *)
  | Left    (* < *)
  | Center  (* = *)
  | Justify (* <> *)
type element =
  | Phrase  of phrase
  | Element of element
type block =
  | Header1    of (attr list * phrase) (* h1. *)
  | Header2    of (attr list * phrase) (* h2. *)
  | Header3    of (attr list * phrase) (* h3. *)
  | Blockquote of (attr list * phrase) (* bq. *)
  | Footnote   of (attr list * phrase) (* fnn. *) (* FIXME *)
  | Paragraph  of (attr list * phrase) (* p. *)
  | Blockcode  of (attr list * phrase) (* bc. *)
  | Pre        of (attr list * phrase) (* pre. *)
  | Numlist    of element list (* # *)
  | Bulllist   of element list (* * *)
  (*| Table of FIXME *)

val teststream: string Stream.t

val parse : string Stream.t -> block Stream.t
