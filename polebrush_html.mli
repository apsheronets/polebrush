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

(** Facilities for converting polebrush into html *)

(** This module uses type [Enum.t] from ExtLib as a more powerful way to work with streams. *)

(** Raises only when function receives invalid AST, for example, with [Header 10]. *)
exception Invalid_polebrush of string

(** Type of Table of Contents, where each line is a tuple of header's id, header's level and header itself *)
type toc = (string * int * Polebrush.line list) list

(** Consumes the enumeration and returns a Table of Contents and enumeration with additionaly id's in headers *)
val toc_of_enum : Polebrush.block Enum.t -> toc * Polebrush.block Enum.t

val elements_of_toc : toc -> Polebrush.element list

(** @param disable_toc Do not generate Tables of Contents. Accelerates outputing and makes it streaming. If true, all [Polebrush.ToC] will be ignored. Default if false.
    @param escape_cdata Do not escape spedial HTML chars. Default is false.
    @param escape_nomarkup Escape special HTML chars even in [==nomarkup==] and [nomarkup.]. Default is false. *)
val of_enum : ?disable_toc:bool -> ?escape_cdata:bool -> ?escape_nomarkup:bool -> Polebrush.block Enum.t -> string Enum.t

(** The same, but do stream processing and don't do tables of Contents, just ignore it.
    @param escape_cdata Do not escape spedial HTML chars. Default is false.
    @param escape_nomarkup Escape special HTML chars even in [==nomarkup==] and [nomarkup.]. Default is false. *)
val of_stream : ?escape_cdata:bool -> ?escape_nomarkup:bool -> Polebrush.block Stream.t -> string Stream.t

(** The same, but takes only one polebrush block.
    @param toc Table of Contents which will be used if [toc.] will attended. If not passed, all [toc.] will be ignored.
    @param escape_cdata Do not escape spedial HTML chars. Default is false.
    @param escape_nomarkup Escape special HTML chars even in [==nomarkup==] and [nomarkup.]. Default is false. *)
val of_block : ?toc:toc -> ?escape_cdata:bool -> ?escape_nomarkup:bool -> Polebrush.block -> string

(** Example of use:

{[
let () =
  let to_lines path =
    let chan = open_in path in
    Stream.from
      (fun _ ->
        try Some (input_line chan)
        with End_of_file -> None) in
  let lines = to_lines "test.txt" in
  let polebrush = Polebrush_pasrer.of_stream lines in
  Stream.iter print_endline (Polebrush_html.of_block polebrush)
]} *)

val of_line : ?escape_cdata:bool -> ?escape_nomarkup:bool -> Polebrush.line -> string

