type token =
  String of string 
  | Int of int
  | Keyword of string
  | OpenParenthesis
  | CloseParenthesis
  | SemiColon
  | Percentages
  | Quote

let token_to_string = function
  | String s -> s
  | Int i -> string_of_int i
  | Keyword s -> s
  | OpenParenthesis -> "(" 
  | CloseParenthesis -> ")"
  | SemiColon -> ";"
  | Percentages -> "%"
  | Quote -> "\""

let string_to_token = function
  | "nil" -> Keyword "nil"
  | "cons" -> Keyword "cons"
  | "read" -> Keyword "read"
  | "while" -> Keyword "while"
  | "do" -> Keyword "do"
  | "od" -> Keyword "od"
  | "write" -> Keyword "write"
  | s -> try Int (int_of_string s) with 
    | Failure _ -> String s 