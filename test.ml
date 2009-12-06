open Parser

let (>>) f g = g f

(*let _ = List.iter (print_char) (Parser.parse Parser.chlist)*)
let () = Parser.test
