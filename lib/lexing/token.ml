type token =
  String of string 
  | Int of int
  | Var of string
  | Keyword of string
  | OpenParenthesis
  | CloseParenthesis
  | SemiColon
  | Percentages
  | Quote
  | Assignement
  | Null

let token_to_string = function
  | String s -> s
  | Int i -> string_of_int i
  | Keyword s -> s
  | OpenParenthesis -> "(" 
  | CloseParenthesis -> ")"
  | SemiColon -> ";"
  | Percentages -> "%"
  | Quote -> "\""
  | Assignement -> ":="
  | Null -> ""
  | Var s -> s

let string_to_token = function
  | "nil" -> Keyword "nil"
  | "cons" -> Keyword "cons"
  | "read" -> Keyword "read"
  | "while" -> Keyword "while"
  | "do" -> Keyword "do"
  | "od" -> Keyword "od"
  | "write" -> Keyword "write"
  | ":=" -> Assignement
  | s -> try Int (int_of_string s) with 
    | Failure _ -> Null