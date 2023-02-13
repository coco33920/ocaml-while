type token =
  | String of string
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
  | "hd" -> Keyword "hd"
  | "tl" -> Keyword "tl"
  | ":=" -> Assignement
  | s -> ( try Int (int_of_string s) with Failure _ -> Null)

let print_token = function
  | String s -> "[String " ^ s ^ "] "
  | Int i -> "[Int " ^ string_of_int i ^ "] "
  | Keyword s -> "[Keyword " ^ s ^ "] "
  | OpenParenthesis -> "[(] "
  | CloseParenthesis -> "[)] "
  | SemiColon -> "[;] "
  | Percentages -> "[%] "
  | Quote -> "[\"] "
  | Assignement -> "[:=] "
  | Null -> ""
  | Var s -> "[Var " ^ s ^ "] "
