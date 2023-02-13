let is_a_token s =
  let t = Token.string_to_token s in
  match t with Null -> (Token.Null, false) | s -> (s, true)

let lex str =
  let l_seq = List.of_seq (String.to_seq str) in
  let rec aux acc word lst =
    match lst with
    | [] -> acc
    | '(' :: t -> aux (Token.OpenParenthesis :: acc) word t
    | ')' :: t ->
        print_endline word;
        if word == "" then aux (Token.CloseParenthesis :: acc) "" t
        else aux (Token.CloseParenthesis :: Token.String word :: acc) "" t
    | '"' :: t -> aux (Token.Quote :: acc) word t
    | '%' :: t -> aux (Token.Percentages :: acc) word t
    | ' ' :: t ->
        let a, b = is_a_token word in
        if b then aux (a :: acc) "" t
        else if word = "" then aux acc "" t
        else aux (Token.String word :: acc) "" t
    | h :: t ->
        let a, b = is_a_token (word ^ String.make 1 h) in
        if b then aux (a :: acc) "" t else aux acc (word ^ String.make 1 h) t
  in
  List.rev (aux [] "" l_seq)
