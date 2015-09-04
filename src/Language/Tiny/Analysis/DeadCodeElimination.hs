module Language.Tiny.Analysis.DeadCodeElimination where

import qualified Data.Char as Char

import Language.Tiny.Analysis.Grammar

eliminateDeadCodeProgram :: Program -> Program
eliminateDeadCodeProgram (Program declarations) = Program $ fmap eliminateDeadCodeDeclaration declarations

eliminateDeadCodeDeclaration :: Declaration -> Declaration
eliminateDeadCodeDeclaration (FunctionDeclaration annotation name parameters block) = FunctionDeclaration annotation name parameters $ eliminateDeadCodeBlock block
eliminateDeadCodeDeclaration declaration = declaration

eliminateDeadCodeBlock :: Block -> Block
eliminateDeadCodeBlock (Block variables statements) = Block variables $ fmap eliminateDeadCodeStatement statements

eliminateDeadCodeStatement :: Statement -> Statement
eliminateDeadCodeStatement conditional@(Conditional expression consequent) =
  if never expression
    then
      Nop
    else if always expression
      then
        consequent
      else
        conditional
eliminateDeadCodeStatement conditional@(Alternative expression consequent alternative) =
  if never expression
    then
      alternative
    else if always expression
      then
        consequent
      else
        conditional
eliminateDeadCodeStatement loop@(Loop expression body) =
  if never expression
    then
      Nop
    else
      loop
eliminateDeadCodeStatement statement = statement


always :: Expression -> Bool
always (IntValue n) | n /= 0 = True
always (CharValue n) | n /= (Char.chr 0) = True
always _ = False

never :: Expression -> Bool
never (IntValue 0) = True
never (CharValue c) = Char.ord c == 0
never _ = False
