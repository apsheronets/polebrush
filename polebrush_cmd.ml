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

let get_header_from_polebrush pb =
  match Enum.peek pb with
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
  let light           = ref false in
  let disable_toc     = ref false in
  let get_header      = ref false in
  let print_header    = ref false in

  let l = [
    "-escape-html", Arg.Set escape_html, "Escape html among markup";
    "-escape-nomarkup", Arg.Set escape_nomarkup, "Escape html in 'nomarkup.' and '== =='";
    "-light", Arg.Set light, "Light mode: no blocks, just lines";
    "-disable-toc", Arg.Set disable_toc, "Disable Tables of Contents, so all 'toc.' will be ignored; set it if you want stream processing";
    "-get-header",  Arg.Set get_header, "Only try to get header of page";
    "-print-header", Arg.Set print_header, "Additionally write 'Header: ...\\n\\n' before parsed text";
  ] in
  Arg.parse l (fun _ -> raise (Arg.Bad help)) help;

  let text = Stream.from (fun _ ->
    try
      let l = read_line () in
      (* FIXME: checking for \r *)
      let l =
        if l.[(String.length l) - 1] = '\r'
        then String.sub l 0 ((String.length l) - 1)
        else l in
      Some l
    with
      End_of_file -> None) in

  if !light
  then
    let first_line = ref true in
    Stream.iter (fun s ->
      if !first_line
      then first_line := false
      else print_string "<br/>";
      let line = Polebrush_parser.line_of_string s in
      let s =
        Polebrush_html.of_line
          ~escape_cdata:(!escape_html)
          ~escape_nomarkup:(!escape_nomarkup)
          line in
      print_string s) text;
    exit 0
  else

  let pb = Polebrush_parser.enum text in

  if !get_header
  then (
    match get_header_from_polebrush pb with
    | Some h -> print_endline h; exit 0
    | None -> exit 1
  ) else (
    if !print_header
    then (
      print_string "Header: ";
      (match get_header_from_polebrush pb with
      | Some h -> print_string h
      | None -> ());
      print_newline ();
      print_newline ());
    let xhtml =
      Polebrush_html.of_enum
        ~disable_toc:(!disable_toc)
        ~escape_cdata:(!escape_html)
        ~escape_nomarkup:(!escape_nomarkup)
        pb in
    Enum.iter (print_string) xhtml;
    exit 0
  )
