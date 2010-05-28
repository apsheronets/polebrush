(** addition for [Textile]: ocamlduce XHTML support for ocsigen *)

(** Raises only when function receives invalid AST, for example, with [Header 10]. *)
exception Invalid_textile of string

val xhtml_of_block : Textile.block -> Xhtmltypes_duce.block

val xhtml_of_textile : Textile.block Stream.t ->
  {{ [ Xhtmltypes_duce.block* ] }}

val xhtml_of_textile_list : Textile.block list ->
  {{ [ Xhtmltypes_duce.block* ] }}
