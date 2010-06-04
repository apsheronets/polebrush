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

(** addition for [Textile]: ocamlduce XHTML support for ocsigen *)

(** Raises only when function receives invalid AST, for example, with [Header 10]. *)
exception Invalid_textile of string

val xhtml_of_block : Textile.block -> Xhtmltypes_duce.block

val xhtml_of_textile : Textile.block Stream.t -> Xhtmltypes_duce.blocks
