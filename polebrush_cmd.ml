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

let get_header_from_polebrush polebrush =
  match Stream.peek polebrush with
  | Some (Polebrush.Header (_, (_, l::_))) ->
      Some (Polebrush.string_of_line l)
  | _ -> None

let () =

  let help =
    "polebrush markup language formatter\n" ^
    "usage: polebrush [OPTIONS]\n" ^
    "example: echo '*hello*' | polebrush" in

  let escape_html     = ref false in
  let escape_nomarkup = ref false in
  let get_header      = ref false in
  let print_header    = ref false in

  let l = [
    "-escape-html", Arg.Set escape_html, "Escape html among markup";
    "-escape-nomarkup", Arg.Set escape_nomarkup, "Escape html in 'nomarkup.' and '== =='";
    "-get-header",  Arg.Set get_header, "Only try to get header of page";
    "-print-header", Arg.Set print_header, "Additionally write 'Header: ...\\n\n' before parsed text";
  ] in
  Arg.parse l (fun _ -> raise (Arg.Bad help)) help;

  let text = Stream.from (fun _ -> try Some (read_line ())
    with End_of_file -> None) in

  let polebrush = Polebrush_parser.of_stream text in

  if !get_header
  then (
    match get_header_from_polebrush polebrush with
    | Some h -> print_endline h; exit 0
    | None -> exit 1
  ) else (
    if !print_header
    then (
      print_string "Header: ";
      (match get_header_from_polebrush polebrush with
      | Some h -> print_string h
      | None -> ());
      print_newline ();
      print_newline ());
    let xhtml =
      Polebrush_html.of_stream
        ~escape_cdata:(!escape_html)
        ~escape_nomarkup:(!escape_nomarkup)
        polebrush in
    Stream.iter (print_string) xhtml;
    exit 0
  )
