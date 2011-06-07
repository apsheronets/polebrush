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
 * Copyright 2011 Alexander Markov *)

let text = Stream.from (fun _ -> try Some (read_line ())
  with End_of_file -> None)

let textile = Textile_parser.of_stream text

let xhtml = Stream.from (fun _ ->
  try Some (Textile_duce.xhtml_of_block (Stream.next textile))
  with Stream.Failure -> None)

let print_xhtml =
  Xhtmlpretty_duce.pretty_print_xhtml
    print_string

let _ =
  Stream.iter (print_xhtml) xhtml;
  exit 0
