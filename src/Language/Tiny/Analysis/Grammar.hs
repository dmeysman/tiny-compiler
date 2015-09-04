module Language.Tiny.Analysis.Grammar where

data Program
  = Program [Declaration]
  deriving (Eq, Show)

data Declaration
  = FunctionDeclaration Annotation String [Parameter] Block
  | VariableDeclaration Variable
  deriving (Eq, Show)

data Variable
  = Variable Annotation String
  deriving (Eq, Show)

data Parameter
  = Parameter Annotation String
  deriving (Eq, Show)

data Annotation
  = IntAnnotation
  | CharAnnotation
  | ArrayAnnotation Annotation Expression
  deriving (Eq, Ord, Show)

data Block
  = Block [Variable] [Statement]
  deriving (Eq, Show)

data Statement
  = Conditional Expression Statement
  | Alternative Expression Statement Statement
  | Loop Expression Statement
  | Statements Block
  | Assignment Lefthand Expression
  | ArrayAssignment Lefthand Expression
  | ReturnStatement Expression
  | VoidCall String [Expression]
  | Output Expression
  | Input Lefthand
  | Nop
  deriving (Eq, Show)

data Lefthand
  = ScalarAccess String
  | ArrayAccess Lefthand Expression
  deriving (Eq, Ord, Show)

data Expression
  = Lefthand Lefthand
  | Add Expression Expression
  | Subtract Expression Expression
  | Multiply Expression Expression
  | Divide Expression Expression
  | Equality Expression Expression
  | Inequality Expression Expression
  | GreaterThan Expression Expression
  | LessThan Expression Expression
  | Negate Expression
  | Not Expression
  | IntValue Int
  | Call String [Expression]
  | CharValue Char
  | ArrayLength Lefthand
  deriving (Eq, Ord, Show)
