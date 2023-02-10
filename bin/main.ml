let () = print_endline "Hello, World!"
let s = Ocaml_while.Lexer.lex "(cons (hd X))";;

List.map (fun c -> print_string (Ocaml_while.Token.print_token c)) s;;