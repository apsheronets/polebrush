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
 * Copyright 2009 Alexander Markov *)

let read_file file =
  let channel = open_in file in
  let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
        try Some (input_line channel)
        with End_of_file -> None) in
  line_stream_of_channel channel

let teststream = read_file "test.txt"

let a = Textile.of_stream teststream

let b = Textile.to_xhtml a

(* make && ocaml /usr/lib/ocaml/extlib/extLib.cma textile.cma -init test.ml *)
