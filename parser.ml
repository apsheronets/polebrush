open ExtLib

let (@@) = List.rev_append

(* "abc" -> ['a', 'b', 'c'] *)
let explode s =
  let rec loop acc n =
    try
      loop ((String.get s n) :: acc) (n+1)
    with Invalid_argument "index out of bounds" -> acc
  in loop [] 0

let read_file file =
  let chan = open_in file in
  let rec loop acc =
    try
      let result = input_char chan in
      loop (result::acc)
    with End_of_file -> acc
  in loop []

let chlist = read_file "test.txt"

let sym = "_"

let rep = "<em>"

let parse l =
  let sym_l = explode sym in
  let rep_l = explode rep in
  let rec loop l acc =
    match l with
    | h::t ->
        if h = (List.hd sym_l)
        then loop t (rep_l @@ acc)
        else loop t (h::acc)
    | [] -> acc
  in loop l []
