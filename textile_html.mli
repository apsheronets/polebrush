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

(** Facilities for converting textile into html *)

(** Function will not escape special HTML chars if [escape] is false. Default is true. *)
val of_stream : ?escape:bool -> Textile.block Stream.t -> string Stream.t

(** The same, but takes one textile block. *)
val of_block : ?escape:bool -> Textile.block -> string

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
  let textile = Textile.of_stream lines in
  Stream.iter print_endline (Textile_html.of_block textile)
]} *)

