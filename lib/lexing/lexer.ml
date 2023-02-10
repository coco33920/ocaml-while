let is_a_token s = 
  let t = Token.string_to_token s in 
  match t with
    | Null -> (Token.Null,false)
    | s -> (s,true)

let lex str = 
  let l_seq = List.of_seq (String.to_seq str) in
  let rec aux acc word lst = 
    let a,b = is_a_token (word) in
    match lst with
      | [] -> acc
      | '('::t -> if b then aux (a::Token.OpenParenthesis::acc) "" t else aux (Token.OpenParenthesis::acc) "" t
      | ')'::t -> if b then aux (a::Token.CloseParenthesis::acc) "" t else aux (Token.CloseParenthesis::acc) "" t
      | '"'::t -> if b then aux (a::Token.Quote::acc) "" t else aux (Token.Quote::acc) "" t
      | '%'::t -> if b then aux (a::Token.Percentages::acc) "" t else aux (Token.Percentages::acc) "" t
      | ' '::t -> if b then aux (a::acc) "" t else aux acc "" t
      | h::t -> let a,b = is_a_token (word^(String.make 1 h)) in if b then aux (a::acc) "" t else aux acc (word^(String.make 1 h)) t
  in List.rev (aux [] "" l_seq)