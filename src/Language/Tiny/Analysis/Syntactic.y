{
module Language.Tiny.Analysis.Syntactic where

import Language.Tiny.Analysis.Token
import Language.Tiny.Analysis.Grammar
}

%name happyParseTokens
%tokentype { Token }

%token

  int { Int }
  if { If }
  else { Else }
  '!=' { ExclamationEquals }
  return { Return }
  '(' { LeftParenthesis }
  ')' { RightParenthesis }
  '{' { LeftBrace }
  '}' { RightBrace }
  '[' { LeftBracket }
  ']' { RightBracket }
  '==' { DoubleEquals }
  '=' { Equals }
  ';' { Semicolon }
  ',' { Comma }
  '+' { Plus }
  '-' { Minus }
  '*' { Asterisk }
  '/' { Slash }
  char { Char }
  write { Write }
  read { Read }
  '>' { Greater }
  '<' { Less }
  '!' { Exclamation }
  length { Length }
  while { While }
  name { Name $$ }
  intValue { LiteralInt $$ }
  charValue { LiteralChar $$ }

%nonassoc '<' '>'
%nonassoc '==' '!='
%left '+' '-'
%left '*' '/'
%right not negate

%right then else

%%

Program : Declarations { Program $1 }

Declarations : Declaration { [$1] }
             | Declarations Declaration { $2 : $1 }

Declaration : FunctionDeclaration { $1 }
            | VariableDeclaration { VariableDeclaration $1 }

FunctionDeclaration : Annotation name '(' Parameters ')' Block { FunctionDeclaration $1 $2 $4 $6 }

Parameters : {- empty -} { [] }
           | NonEmptyParameters { $1 }

NonEmptyParameters : Parameter { [$1] }
                   | Parameter ',' NonEmptyParameters { $1 : $3 }

Parameter : Annotation name { Parameter $1 $2 }

Block : '{' VariableDeclarations Statements '}' { Block $2 $3 }

VariableDeclarations : {- empty -} { [] }
                     | NonEmptyVariableDeclarations { $1 }

NonEmptyVariableDeclarations : VariableDeclaration { [$1] }
                             | VariableDeclaration NonEmptyVariableDeclarations { $1 : $2 }

VariableDeclaration : Annotation name ';' { Variable $1 $2 }

Annotation : int { IntAnnotation }
           | char { CharAnnotation }
           | Annotation '[' Expression ']' { ArrayAnnotation $1 $3 }

Statements : Statement { [$1] }
           | Statements Statement { $2 : $1 }

Statement : if '(' Expression ')' Statement %prec then { Conditional $3 $5 }
          | if '(' Expression ')' Statement else Statement { Alternative $3 $5 $7 }
          | while '(' Expression ')' Statement { Loop $3 $5 }
          | Lefthand '=' Expression ';' { Assignment $1 $3 }
          | return Expression ';' { ReturnStatement $2 }
          | name '(' Arguments ')' ';' { VoidCall $1 $3 }
          | Block { Statements $1 }
          | write Expression ';' { Output $2 }
          | read Lefthand ';' { Input $2 }

Lefthand : name { ScalarAccess $1 }
         | Lefthand '[' Expression ']' { ArrayAccess $1 $3 }

Expression : Lefthand { Lefthand $1 }
           | Expression '+' Expression { Add $1 $3 }
           | Expression '-' Expression { Subtract $1 $3 }
           | Expression '*' Expression { Multiply $1 $3 }
           | Expression '/' Expression { Divide $1 $3 }
           | Expression '==' Expression { Equality $1 $3 }
           | Expression '!=' Expression { Inequality $1 $3 }
           | Expression '<' Expression { LessThan $1 $3 }
           | Expression '>' Expression { GreaterThan $1 $3 }
           | '-' Expression %prec negate { Negate $2 }
           | '!' Expression %prec not { Not $2 }
           | '(' Expression ')' { $2 }
           | intValue { IntValue $1 }
           | name '(' Arguments ')' { Call $1 $3 }
           | charValue { CharValue $1 }
           | length Lefthand { ArrayLength $2 }

Arguments : Expression { [$1] }
          | Arguments ',' Expression { $3 : $1 }

{
happyError = error . ("syntactic error: " ++) . show
}
