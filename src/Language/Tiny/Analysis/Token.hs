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
  deriving (Eq, Show)
