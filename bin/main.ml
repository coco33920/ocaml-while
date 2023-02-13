let () = print_endline "Hello, World!"

let sd =
  Ocaml_while.Utils.read_file "examples/test.while"
  |> String.concat " " |> Ocaml_while.Lexer.lex
;;

List.map (fun c -> print_string (Ocaml_while.Token.print_token c)) sd
