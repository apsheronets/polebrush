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

(** Facilities for converting polebrush into TyXML's [Xhtml_types_duce] representation *)

(** Raises only when function receives invalid AST, for example, with [Header 10]. *)
exception Invalid_polebrush of string

val xhtml_of_block : ?toc:Polebrush_html.toc -> Polebrush.block -> Xhtml_types_duce.blocks

val of_enum : ?disable_toc:bool -> Polebrush.block Enum.t -> Xhtml_types_duce.blocks Enum.t

val of_stream : Polebrush.block Stream.t -> Xhtml_types_duce.blocks Stream.t

val xhtml_of_enum : ?disable_toc:bool -> Polebrush.block Enum.t -> Xhtml_types_duce.blocks

val xhtml_of_stream : Polebrush.block Stream.t -> Xhtml_types_duce.blocks
