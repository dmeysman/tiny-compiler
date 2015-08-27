module Language.Tiny.Analysis.Token where

data Token
  = Name String
  | LiteralInt Int
  | LiteralChar Char
  | Int
  | If
  | Else
  | ExclamationEquals
  | Return
  | LeftParenthesis
  | RightParenthesis
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Equals
  | Semicolon
  | Comma
  | Plus
  | Minus
  | Asterisk
  | Slash
  | DoubleEquals
  | Char
  | Write
  | Read
  | Greater
  | Less
  | Exclamation
  | Length
  | While
  deriving Eq

instance Show Token where

  show (Name s) = s
  show (LiteralInt n) = show n
  show (LiteralChar c) = show c
  show Int = "int"
  show If = "if"
  show Else = "else"
  show ExclamationEquals = "!="
  show Return = "return"
  show LeftParenthesis = "("
  show RightParenthesis = ")"
  show LeftBrace = "{"
  show RightBrace = "}"
  show LeftBracket = "["
  show RightBracket = "]"
  show Equals = "="
  show Semicolon = ";"
  show Comma = ","
  show Plus = "+"
  show Minus = "-"
  show Asterisk = "*"
  show Slash = "/"
  show DoubleEquals = "=="
  show Char = "char"
  show Write = "write"
  show Read = "read"
  show Greater = ">"
  show Less = "<"
  show Exclamation = "!"
  show Length = "length"
  show While = "while"
