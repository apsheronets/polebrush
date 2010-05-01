
exception Invalid_textile of string

val xhtml_of_block : Textile.block -> Xhtmltypes_duce.block

val xhtml_of_textile : Textile.block Stream.t ->
  {{ [ Xhtmltypes_duce.block* ] }}
