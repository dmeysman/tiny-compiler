{
module Language.Tiny.Analysis.Lexical where

import Language.Tiny.Analysis.Token
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-z]
$alphanumeric = [$alpha$digit]
$alphanumericUnderscore = [$alphanumeric\_]

@comment = "//" .*
@name = $alpha $alphanumericUnderscore*
@literalInt = $digit+
@literalChar = \' . \'

tokens :-

  $white+ ;
  @comment ;
  "int" { const Int }
  "if" { const If }
  "else" { const Else }
  "!=" { const ExclamationEquals }
  "return" { const Return }
  "(" { const LeftParenthesis }
  ")" { const RightParenthesis }
  "{" { const LeftBrace }
  "}" { const RightBrace }
  "[" { const LeftBracket }
  "]" { const RightBracket }
  "==" { const DoubleEquals }
  "=" { const Equals }
  ";" { const Semicolon }
  "," { const Comma }
  "+" { const Plus }
  "-" { const Minus }
  "*" { const Asterisk }
  "/" { const Slash }
  "char" { const Char }
  "write" { const Write }
  "read" { const Read }
  ">" { const Greater }
  "<" { const Less }
  "!" { const Exclamation }
  "length" { const Length }
  "while" { const While }
  @name { \s -> Name s }
  @literalInt { \s -> LiteralInt (read s) }
  @literalChar { \s -> LiteralChar (head (tail s)) }
