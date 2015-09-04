{-# LANGUAGE Rank2Types #-}

module Language.Tiny.Analysis.ConstantFolding where

import qualified Data.Char as Char

import Language.Tiny.Analysis.Grammar

foldConstantsProgram :: Program -> Program
foldConstantsProgram (Program declarations) = Program $ fmap foldConstantsDeclaration declarations

foldConstantsDeclaration :: Declaration -> Declaration
foldConstantsDeclaration (FunctionDeclaration annotation name parameters block) = FunctionDeclaration annotation name parameters $ foldConstantsBlock block
foldConstantsDeclaration declaration = declaration

foldConstantsBlock :: Block -> Block
foldConstantsBlock (Block variables statements) = Block variables $ fmap foldConstantsStatement statements

foldConstantsStatement :: Statement -> Statement
foldConstantsStatement (Conditional expression statement) = Conditional (foldConstantsExpression expression) (foldConstantsStatement statement)
foldConstantsStatement (Alternative expression statement statement') = Alternative (foldConstantsExpression expression) (foldConstantsStatement statement) (foldConstantsStatement statement')
foldConstantsStatement (Loop expression statement) = Loop (foldConstantsExpression expression) (foldConstantsStatement statement)
foldConstantsStatement (Statements block) = Statements (foldConstantsBlock block)
foldConstantsStatement (Assignment lefthand expression) = Assignment lefthand (foldConstantsExpression expression)
foldConstantsStatement (ReturnStatement expression) = ReturnStatement (foldConstantsExpression expression)
foldConstantsStatement (VoidCall name expressions) = VoidCall name (fmap foldConstantsExpression expressions)
foldConstantsStatement (Output expression) = Output (foldConstantsExpression expression)
foldConstantsStatement statement = statement

foldConstantsExpression :: Expression -> Expression
foldConstantsExpression (Add (IntValue 0) expression) = foldConstantsExpression expression
foldConstantsExpression (Add expression (IntValue 0)) = foldConstantsExpression expression
foldConstantsExpression (Add expression expression') = combineIntWith (+) Add (foldConstantsExpression expression) (foldConstantsExpression expression')
foldConstantsExpression (Subtract (IntValue 0) expression) = Negate $ foldConstantsExpression expression
foldConstantsExpression (Subtract expression (IntValue 0)) = foldConstantsExpression expression
foldConstantsExpression (Subtract expression expression') = combineIntWith (-) Subtract (foldConstantsExpression expression) (foldConstantsExpression expression')
foldConstantsExpression (Multiply (IntValue 1) expression) = foldConstantsExpression expression
foldConstantsExpression (Multiply expression (IntValue 1)) = foldConstantsExpression expression
foldConstantsExpression (Multiply expression expression') = combineIntWith (*) Multiply (foldConstantsExpression expression) (foldConstantsExpression expression')
foldConstantsExpression (Divide expression (IntValue 1)) = foldConstantsExpression expression
foldConstantsExpression (Divide expression expression') = combineIntWith div Divide (foldConstantsExpression expression) (foldConstantsExpression expression')
foldConstantsExpression (Equality expression expression') = combineBoolWith (==) Equality (foldConstantsExpression expression) (foldConstantsExpression expression')
foldConstantsExpression (Inequality expression expression') = combineBoolWith (/=) Inequality (foldConstantsExpression expression) (foldConstantsExpression expression')
foldConstantsExpression (LessThan expression expression') = combineBoolWith (<) LessThan (foldConstantsExpression expression) (foldConstantsExpression expression')
foldConstantsExpression (GreaterThan expression expression') = combineBoolWith (>) GreaterThan (foldConstantsExpression expression) (foldConstantsExpression expression')
foldConstantsExpression (Negate expression) = additiveInverse (foldConstantsExpression expression)
foldConstantsExpression (Not expression) = negation (foldConstantsExpression expression)
foldConstantsExpression (Call name expressions) = Call name (fmap foldConstantsExpression expressions)
foldConstantsExpression expression = expression

combineIntWith :: (Int -> Int -> Int) -> (Expression -> Expression -> Expression) -> Expression -> Expression -> Expression
combineIntWith now _ (IntValue int) (IntValue int') = (IntValue (now int int'))
combineIntWith now _ (CharValue char) (CharValue char') = (CharValue (Char.chr (now (Char.ord char) (Char.ord char'))))
combineIntWith now _ (IntValue int) (CharValue char) = (IntValue (now int (Char.ord char)))
combineIntWith now _ (CharValue char) (IntValue int) = (IntValue (now (Char.ord char) int))
combineIntWith _ later expression expression' = later expression expression'

combineBoolWith :: (forall a. (Ord a) => a -> a -> Bool) -> (Expression -> Expression -> Expression) -> Expression -> Expression -> Expression
combineBoolWith now _ (IntValue int) (IntValue int') = (IntValue (boolToInt (now int int')))
combineBoolWith now _ (CharValue char) (CharValue char') = (CharValue (Char.chr (boolToInt (now char char'))))
combineBoolWith now _ (IntValue int) (CharValue char) = (IntValue (boolToInt (now int (Char.ord char))))
combineBoolWith now _ (CharValue char) (IntValue int) = (IntValue (boolToInt (now (Char.ord char) int)))
combineBoolWith _ later expression expression' = later expression expression'

additiveInverse :: Expression -> Expression
additiveInverse (IntValue int) = (IntValue (- int))
additiveInverse expression = Negate expression

negation :: Expression -> Expression
negation (IntValue 0) = (IntValue 1)
negation (IntValue n) = (IntValue 0)
negation expression = Not expression

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1
