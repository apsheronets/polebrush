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

(** Facilities for converting polebrush into ocsigen's [Xhtmltypes_duce] representation *)

(** Raises only when function receives invalid AST, for example, with [Header 10]. *)
exception Invalid_polebrush of string

val xhtml_of_block : Polebrush.block -> Xhtmltypes_duce.block

val xhtml_of_polebrush : Polebrush.block Stream.t -> Xhtmltypes_duce.blocks