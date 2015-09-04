module Language.Tiny.Synthesis.Intermediate where

import qualified Control.Monad.State as State
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Debug.Trace as Trace

import Language.Tiny.Analysis.Grammar
import Language.Tiny.Analysis.Syntactic
import Language.Tiny.PrettyPrint

data IntermediateCode
  = BinaryApplication BinaryOperation Operand Operand Operand
  | UnaryApplication UnaryOperation Operand Operand
  | Assign Operand Operand
  | IndexedAccess Operand Operand Operand
  | IndexedAssign Operand Operand Operand
  | Push Operand
  | Pop Operand
  | GotoFunction Operand String Int
  | Return Operand
  | Label Operand
  | Goto Operand
  | If Operand Operand
  | Global Operand
  | Read Operand
  | WriteInt Operand
  | WriteChar Operand
  | NoInstruction
  deriving (Eq, Ord, Show)

instance PrettyPrint IntermediateCode where

  prettyPrint (BinaryApplication operation operand operand' operand'') = prettyPrint operand ++ " = " ++ prettyPrint operand' ++ " " ++ prettyPrint operation ++ " " ++ prettyPrint operand''
  prettyPrint (UnaryApplication operation operand operand') = prettyPrint operand ++ " = " ++ prettyPrint operation ++ prettyPrint operand'
  prettyPrint (Assign operand operand') = prettyPrint operand ++ " = " ++ prettyPrint operand'
  prettyPrint (IndexedAccess operand operand' operand'') = prettyPrint operand ++ " = " ++ prettyPrint operand' ++ '[' : prettyPrint operand'' ++ "]"
  prettyPrint (IndexedAssign operand operand' operand'') = prettyPrint operand ++ '[' : prettyPrint operand' ++ "] = " ++ prettyPrint operand''
  prettyPrint (Push operand) = "push " ++ prettyPrint operand
  prettyPrint (Pop operand) = "pop " ++ prettyPrint operand
  prettyPrint (GotoFunction operand name int) = prettyPrint operand ++ " = call " ++ name
  prettyPrint (Return operand) = "return " ++ prettyPrint operand
  prettyPrint (Label name) = "label " ++ prettyPrint name
  prettyPrint (Goto name) = "goto " ++ prettyPrint name
  prettyPrint (If operand name) = "branch to " ++ prettyPrint name ++ " if " ++ prettyPrint operand
  prettyPrint (Global name) = prettyPrint name
  prettyPrint (Read name) = "read " ++ prettyPrint name
  prettyPrint (WriteInt name) = "write int " ++ prettyPrint name
  prettyPrint (WriteChar name) = "write char " ++ prettyPrint name
  prettyPrint NoInstruction = ""

data Operand
  = Int Int
  | Char Char
  | LocalEntry SymbolTableEntry
  | GlobalEntry SymbolTableEntry
  | Temporary String
  | Padded Operand Operand
  deriving (Eq, Ord, Show)

instance PrettyPrint Operand where

  prettyPrint (Int int) = '$' : show int
  prettyPrint (Char char) = '$' : (show $ Char.ord char)
  prettyPrint (LocalEntry entry) = prettyPrint entry
  prettyPrint (GlobalEntry entry) = prettyPrint entry
  prettyPrint (Temporary name) = name
  prettyPrint (Padded name int) = prettyPrint name

data BinaryOperation
  = Addition
  | Subtraction
  | Multiplication
  | Division
  | Equal
  | NotEqual
  | Less
  | Greater
  deriving (Eq, Ord, Show)

instance PrettyPrint BinaryOperation where

  prettyPrint Addition = "+"
  prettyPrint Subtraction = "-"
  prettyPrint Multiplication = "*"
  prettyPrint Division = "/"
  prettyPrint Equal = "=="
  prettyPrint NotEqual = "!="
  prettyPrint Less = "<"
  prettyPrint Greater = ">"

data UnaryOperation
  = AdditiveInverse
  | Negation
  | IntToChar
  | CharToInt
  deriving (Eq, Ord, Show)

instance PrettyPrint UnaryOperation where

  prettyPrint AdditiveInverse = "-"
  prettyPrint Negation = "!"
  prettyPrint IntToChar = "int to char "
  prettyPrint CharToInt = "char to int "

arrayLength :: SymbolTableEntry -> Int
arrayLength (VariableEntry _ annotation@(ArrayAnnotation _ _)) = annotationLength annotation
arrayLength _ = 1

annotationLength :: Annotation -> Int
annotationLength (ArrayAnnotation annotation value) = valueOf value * annotationLength annotation
  where
    valueOf (IntValue int) = int
    valueOf (CharValue char) = Char.ord char
annotationLength _ = 1

fresh :: String -> State.State (Int, String, [SymbolTableEntry]) String
fresh prefix =
  do
    (count, scope, table) <- State.get
    State.put (succ count, scope, table)
    return $ prefix ++ show count

freshTemporary :: State.State (Int, String, [SymbolTableEntry]) Operand
freshTemporary =
  do
    temporary <- fresh "_temporary"
    return $ Temporary temporary

freshLabel :: State.State (Int, String, [SymbolTableEntry]) Operand
freshLabel =
  do
    label <- fresh "_label"
    return $ Temporary label

local :: String -> State.State (Int, String, [SymbolTableEntry]) Operand
local name =
  do
    (_, scope, table) <- State.get
    let (Just (FunctionEntry _ _ bindings)) = lookupScope scope table
    orGlobal $ lookupVariable name bindings
  where
    orGlobal (Just entry) = return $ LocalEntry entry
    orGlobal Nothing = global name

global :: String -> State.State (Int, String, [SymbolTableEntry]) Operand
global name =
  do
    (_, _, table) <- State.get
    let (Just entry) = lookupVariableOrScope name table
    return $ GlobalEntry entry

lookupScope :: String -> [SymbolTableEntry] -> Maybe SymbolTableEntry
lookupScope name (entry@(FunctionEntry _ name' _) : entries)
  | name == name' = Just entry
  | otherwise = lookupScope name entries
lookupScope _ _ = Nothing

lookupVariable :: String -> [SymbolTableEntry] -> Maybe SymbolTableEntry
lookupVariable name (entry@(ParameterEntry name' _) : entries)
  | name == name' = Just entry
  | otherwise = lookupVariable name entries
lookupVariable name (entry@(VariableEntry name' _) : entries)
  | name == name' = Just entry
  | otherwise = lookupVariable name entries
lookupVariable _ _ = Nothing

lookupVariableOrScope :: String -> [SymbolTableEntry] -> Maybe SymbolTableEntry
lookupVariableOrScope name (entry@(ParameterEntry name' _) : entries)
  | name == name' = Just entry
  | otherwise = lookupVariable name entries
lookupVariableOrScope name (entry@(VariableEntry name' _) : entries)
  | name == name' = Just entry
  | otherwise = lookupVariableOrScope name entries
lookupVariableOrScope name (entry@(FunctionEntry _ name' _) : entries)
  | name == name' = Just entry
  | otherwise = lookupVariableOrScope name entries
lookupVariableOrScope _ _ = Nothing

enterScope :: String -> State.State (Int, String, [SymbolTableEntry]) ()
enterScope name =
  do
    (count, _, table) <- State.get
    State.put (count, name, table)

removeIdentities :: [IntermediateCode] -> [IntermediateCode]
removeIdentities = filter $ not . identity
  where
    identity (Assign operand operand') = operand == operand'
    identity _ = False

basicBlocks :: [IntermediateCode] -> [[IntermediateCode]]
basicBlocks [] = []
basicBlocks intermediates = block : basicBlocks intermediates'
  where
    (block, intermediates') = untilLeader intermediates

untilLeader :: [IntermediateCode] -> ([IntermediateCode], [IntermediateCode])
untilLeader = untilLeader' False []
  where
    untilLeader' _ consumed [] = (reverse consumed, [])
    untilLeader' afterJump consumeds@(_ : consumeds') consumables@(consumable : consumables')
      | afterJump = (reverse consumeds, consumables)
      | leader consumable = ((reverse consumeds), consumables)
      | otherwise = untilLeader' (jump consumable) (consumable : consumeds) consumables'
    untilLeader' _ [] (consumable : consumables) = untilLeader' False [consumable] consumables
    leader (Label _) = True
    leader _ = False
    jump (GotoFunction _ _ _) = True
    jump (Return _) = True
    jump (Goto _) = True
    jump (If _ _) = True
    jump _ = False

programToIntermediate :: Program -> [SymbolTableEntry] -> [[IntermediateCode]]
programToIntermediate (Program declarations) table = basicBlocks . removeIdentities $ State.evalState (declarationsToIntermediate declarations) (0, "", table)

declarationsToIntermediate :: [Declaration] -> State.State (Int, String, [SymbolTableEntry]) [IntermediateCode]
declarationsToIntermediate [] = return []
declarationsToIntermediate (declaration : declarations) =
  do
    intermediate <- declarationToIntermediate declaration
    intermediates <- declarationsToIntermediate declarations
    return $ intermediates ++ intermediate

declarationToIntermediate :: Declaration -> State.State (Int, String, [SymbolTableEntry]) [IntermediateCode]
declarationToIntermediate (FunctionDeclaration _ name parameters block) =
  do
    entry <- global name
    enterScope name
    intermediate <- parametersToIntermediate parameters
    intermediate' <- blockToIntermediate block
    return $ Label entry : intermediate ++ intermediate'
declarationToIntermediate (VariableDeclaration (Variable _ name)) =
  do
    entry <- global name
    return [Global entry]

parametersToIntermediate :: [Parameter] -> State.State (Int, String, [SymbolTableEntry]) [IntermediateCode]
parametersToIntermediate [] = return []
parametersToIntermediate ((Parameter _ name) : parameters) =
  do
    entry <- local name
    intermediates <- parametersToIntermediate parameters
    return $ Pop entry : intermediates

statementToIntermediate :: Statement -> State.State (Int, String, [SymbolTableEntry]) [IntermediateCode]
statementToIntermediate (Conditional expression statement) =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- statementToIntermediate statement
    consequent <- freshLabel
    end <- freshLabel
    return $ intermediate ++ [If (result intermediate) consequent, Goto end, Label consequent] ++ intermediate' ++ [Label end]
statementToIntermediate (Alternative expression statement statement') =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- statementToIntermediate statement
    intermediate'' <- statementToIntermediate statement'
    consequent <- freshLabel
    alternative <- freshLabel
    end <- freshLabel
    return $ intermediate ++ [If (result intermediate) consequent, Goto alternative, Label consequent] ++ intermediate' ++ [Goto end, Label alternative] ++ intermediate'' ++ [Label end]
statementToIntermediate (Loop expression statement) =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- statementToIntermediate statement
    begin <- freshLabel
    consequent <- freshLabel
    end <- freshLabel
    return $ Label begin : intermediate ++ [If (result intermediate) consequent, Goto end, Label consequent] ++ intermediate' ++ [Goto begin, Label end]
statementToIntermediate (Statements block) = blockToIntermediate block
statementToIntermediate (Assignment lefthand expression) =
  do
    intermediate <- lefthandToIntermediate lefthand
    intermediate' <- expressionToIntermediate expression
    return $ intermediate ++ intermediate' ++ [Assign (result intermediate) (result intermediate')]
statementToIntermediate (ArrayAssignment lefthand expression) =
  do
    entry' <- local $ name lefthand
    let annotation = entry entry'
    intermediate <- expressionToIntermediate expression
    return $ intermediate ++ [IndexedAssign entry' (Int $ 4 * (index (reverse (indexes lefthand)) (reverse (lengths annotation)))) (result intermediate)]
  where
    entry (GlobalEntry (VariableEntry _ annotation')) = annotation'
    entry (LocalEntry (VariableEntry _ annotation')) = annotation'
    entry (LocalEntry (ParameterEntry _ annotation')) = annotation'
    name (ScalarAccess n) = n
    name (ArrayAccess lefthand expression) = name lefthand
    index indices lengths = let (Just index') = List.lookup indices . flip zip [0..] . selectFromRanges $ lengthsToRanges lengths in index'
     where
       lengthsToRanges = map (\l -> [0..l - 1])
       selectFromRanges [] = return []
       selectFromRanges (r : rs) = [i : is | i <- r, is <- selectFromRanges rs]
    indexes (ArrayAccess lefthand expression) = evaluateInt expression : indexes lefthand
    indexes (ScalarAccess _) = []
    lengths (ArrayAnnotation annotation expression) = evaluateInt expression : lengths annotation
    lengths _ = []
statementToIntermediate (ReturnStatement expression) =
  do
    intermediate <- expressionToIntermediate expression
    return $ intermediate ++ [Return (result intermediate)]
statementToIntermediate (VoidCall name expressions) =
  do
    intermediate <- argumentsToIntermediate expressions
    result' <- freshTemporary
    return $ intermediate ++ [GotoFunction result' name (length expressions)]
statementToIntermediate (Output expression) =
  do
    intermediate <- expressionToIntermediate expression
    function <- polymorphicWrite expression
    return $ intermediate ++ [function $ result intermediate]
  where
    polymorphicWrite (CharValue _) = return WriteChar
    polymorphicWrite (Call name _) = do
      (GlobalEntry (FunctionEntry annotation _ _)) <- global name
      if annotation == CharAnnotation then return WriteChar else return WriteInt
    polymorphicWrite _ = return WriteInt
statementToIntermediate (Input lefthand) =
  do
    intermediate <- lefthandToIntermediate lefthand
    return $ intermediate ++ [Read (result intermediate)]
statementToIntermediate Nop = return []

blockToIntermediate :: Block -> State.State (Int, String, [SymbolTableEntry]) [IntermediateCode]
blockToIntermediate (Block _ statements) =
  do
    intermediate <- statementsToIntermediate statements
    return intermediate

statementsToIntermediate :: [Statement] -> State.State (Int, String, [SymbolTableEntry]) [IntermediateCode]
statementsToIntermediate [] = return []
statementsToIntermediate (statement : statements) =
  do
    intermediate <- statementToIntermediate statement
    intermediates <- statementsToIntermediate statements
    return $ intermediates ++ intermediate

expressionToIntermediate :: Expression -> State.State (Int, String, [SymbolTableEntry]) [IntermediateCode]
expressionToIntermediate (Lefthand lefthand) = lefthandToIntermediate lefthand
expressionToIntermediate (Add expression expression') =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- expressionToIntermediate expression'
    result' <- freshTemporary
    return $ intermediate ++ intermediate' ++ [BinaryApplication Addition result' (result intermediate) (result intermediate')]
expressionToIntermediate (Subtract expression expression') =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- expressionToIntermediate expression'
    result' <- freshTemporary
    return $ intermediate ++ intermediate' ++ [BinaryApplication Subtraction result' (result intermediate) (result intermediate')]
expressionToIntermediate (Multiply expression expression') =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- expressionToIntermediate expression'
    result' <- freshTemporary
    return $ intermediate ++ intermediate' ++ [BinaryApplication Multiplication result' (result intermediate) (result intermediate')]
expressionToIntermediate (Divide expression expression') =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- expressionToIntermediate expression'
    result' <- freshTemporary
    return $ intermediate ++ intermediate' ++ [BinaryApplication Division result' (result intermediate) (result intermediate')]
expressionToIntermediate (Equality expression expression') =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- expressionToIntermediate expression'
    result' <- freshTemporary
    return $ intermediate ++ intermediate' ++ [BinaryApplication Equal result' (result intermediate) (result intermediate')]
expressionToIntermediate (Inequality expression expression') =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- expressionToIntermediate expression'
    result' <- freshTemporary
    return $ intermediate ++ intermediate' ++ [BinaryApplication NotEqual result' (result intermediate) (result intermediate')]
expressionToIntermediate (GreaterThan expression expression') =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- expressionToIntermediate expression'
    result' <- freshTemporary
    return $ intermediate ++ intermediate' ++ [BinaryApplication Greater result' (result intermediate) (result intermediate')]
expressionToIntermediate (LessThan expression expression') =
  do
    intermediate <- expressionToIntermediate expression
    intermediate' <- expressionToIntermediate expression'
    result' <- freshTemporary
    return $ intermediate ++ intermediate' ++ [BinaryApplication Less result' (result intermediate) (result intermediate')]
expressionToIntermediate (Negate expression) =
  do
    intermediate <- expressionToIntermediate expression
    result' <- freshTemporary
    return $ intermediate ++ [UnaryApplication AdditiveInverse result' (result intermediate)]
expressionToIntermediate (Not expression) =
  do
    intermediate <- expressionToIntermediate expression
    result' <- freshTemporary
    return $ intermediate ++ [UnaryApplication Negation result' (result intermediate)]
expressionToIntermediate (IntValue int) =
  do
    result' <- freshTemporary
    return [Assign result' (Int int)]
expressionToIntermediate (Call name expressions) =
  do
    intermediate <- argumentsToIntermediate expressions
    result' <- freshTemporary
    return $ intermediate ++ [GotoFunction result' name (length expressions)]
expressionToIntermediate (CharValue char) =
  do
    result' <- freshTemporary
    return [Assign result' (Char char)]
expressionToIntermediate (ArrayLength (ScalarAccess name)) =
  do
    entry' <- local name
    let entry'' = entry entry'
    result' <- freshTemporary
    return [Assign entry' (Int 0), Assign result' (Int (arrayLength entry''))]
  where
    entry (GlobalEntry entry) = entry
    entry (LocalEntry entry) = entry

lefthandToIntermediate :: Lefthand -> State.State (Int, String, [SymbolTableEntry]) [IntermediateCode]
lefthandToIntermediate (ScalarAccess name) =
  do
    entry <- local name
    return [Assign entry entry]
lefthandToIntermediate lefthand@(ArrayAccess _ _) =
  do
    entry' <- local $ name lefthand
    let annotation = entry entry'
    result' <- freshTemporary
    return $ [IndexedAccess result' (Padded entry' (Int $ -4 * (index (reverse (indexes lefthand)) (reverse (lengths annotation))))) (Int $ 4 * (index (reverse (indexes lefthand)) (reverse (lengths annotation))))]
  where
    entry (GlobalEntry (VariableEntry _ annotation)) = annotation
    entry (LocalEntry (VariableEntry _ annotation)) = annotation
    entry (LocalEntry (ParameterEntry _ annotation)) = annotation
    name (ScalarAccess n) = n
    name (ArrayAccess lefthand expression) = name lefthand
    index indices lengths = let (Just index') = List.lookup indices . flip zip [0..] . selectFromRanges $ lengthsToRanges lengths in index'
     where
       lengthsToRanges = map (\l -> [0..l - 1])
       selectFromRanges [] = return []
       selectFromRanges (r : rs) = [i : is | i <- r, is <- selectFromRanges rs]
    indexes (ArrayAccess lefthand expression) = evaluateInt expression : indexes lefthand
    indexes (ScalarAccess _) = []
    lengths (ArrayAnnotation annotation expression) = evaluateInt expression : lengths annotation
    lengths _ = []

argumentsToIntermediate :: [Expression] -> State.State (Int, String, [SymbolTableEntry]) [IntermediateCode]
argumentsToIntermediate [] = return []
argumentsToIntermediate (argument : arguments) =
  do
    intermediate <- argumentToIntermediate argument
    intermediates <- argumentsToIntermediate arguments
    return $ intermediate ++ intermediates

argumentToIntermediate :: Expression -> State.State (Int, String, [SymbolTableEntry]) [IntermediateCode]
argumentToIntermediate argument =
  do
    intermediate <- expressionToIntermediate argument
    return $ intermediate ++ [Push (result intermediate)]

evaluateInt :: Expression -> Int
evaluateInt (IntValue int) = int
evaluateInt (CharValue char) = Char.ord char
evaluateInt _ = error "variable expression, expected constant"

result :: [IntermediateCode] -> Operand
result intermediates = result' $ last intermediates
  where
    result' (BinaryApplication _ operand _ _) = operand
    result' (UnaryApplication _ operand _) = operand
    result' (Assign operand _) = operand
    result' (IndexedAccess operand _ _) = operand
    result' (IndexedAssign _ _ operand) = operand
    result' (GotoFunction operand _ _) = operand
    result' (Return operand) = operand
