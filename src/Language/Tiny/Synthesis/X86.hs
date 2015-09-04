module Language.Tiny.Synthesis.X86 where

import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Debug.Trace as Trace

import Language.Tiny.Analysis.Grammar
import Language.Tiny.Analysis.Syntactic
import Language.Tiny.PrettyPrint
import Language.Tiny.Synthesis.Intermediate

intermediatesToX86 :: [[IntermediateCode]] -> String
intermediatesToX86 intermediates = combine $ State.execState (intermediatesToX86' (concat intermediates)) (Map.empty, Map.empty, Map.empty, 8, Map.empty, -4, "", "", "")
  where
    combine (_, _, _, _, _, _, data', bss, code) = ".section .data\nnl: .long 10\n" ++ data' ++ "\n.section .text\n.globl _start\n.type _start, @function\n_start:\n  call tiny\n  jmp _exit\n" ++ purge code ++ ".type _exit, @function\n_exit:\n  movl %eax, %ebx\n  movl $1, %eax\n  int $0x80\n"
    purge = unlines . filter (\line -> not . null $ words line) . lines

appendData :: String -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
appendData line =
  do
    (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, x86) <- State.get
    State.put (arrays, globals, parameters, parameterOffset, locals, localOffset, (data' ++ line ++ "\n"), bss, x86)

appendDataInline :: String -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
appendDataInline line =
  do
    (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, x86) <- State.get
    State.put (arrays, globals, parameters, parameterOffset, locals, localOffset, (data' ++ line), bss, x86)

appendBss :: String -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
appendBss line =
  do
    (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, x86) <- State.get
    State.put (arrays, globals, parameters, parameterOffset, locals, localOffset, data', (bss ++ line ++ "\n"), x86)

appendIndented :: String -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
appendIndented = appendCode . ("  " ++)

appendCode :: String -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
appendCode line =
  do
    (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, x86) <- State.get
    State.put (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, (x86 ++ line ++ "\n"))

appendInline :: String -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
appendInline line =
  do
    (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, x86) <- State.get
    State.put (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, (x86 ++ line))

clearLocals :: State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
clearLocals =
  do
    (arrays, globals, parameters, parameterOffset, _, _, data', bss, x86) <- State.get
    State.put (arrays, globals, parameters, parameterOffset, Map.empty, -4, data', bss, x86)

clearParameters :: State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
clearParameters =
  do
    (arrays, globals, _, _, locals, localOffset, data', bss, x86) <- State.get
    State.put (arrays, globals, Map.empty, 8, locals, localOffset, data', bss, x86)

insertFrame :: Int -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
insertFrame position =
  do
    (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, x86) <- State.get
    let frameSize = (ceiling (((fromIntegral (- localOffset)) + 8) / 16)) * 16
        x86' = let (ys, zs) = splitAt position (lines x86) in (List.intersperse "\n" ys) ++ ["\n  subl $" ++ show frameSize ++ ", %esp\n"] ++ (List.intersperse "\n" zs)
    State.put (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, concat x86' ++ "\n")

-- registerGlobal :: SymbolTableEntry -> String -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
-- registerGlobal entry string =
--   do
--     (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, x86) <- State.get
--     State.put (arrays, Map.insert entry string globals, parameters, parameterOffset, locals, localOffset, data', bss, x86)

operandToX86 :: Operand -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) (String, String)
operandToX86 (Int int) = return ("", prettyPrint $ Int int)
operandToX86 (Char char) = return ("", prettyPrint $ Char char)
-- operandToX86 (Padded operand@(LocalEntry (ParameterEntry _ _)) (Int int)) =
--   do
--     (_, x86operand) <- operandToX86 operand
--     return ("", x86operand)
-- operandToX86 (Padded operand@(LocalEntry _) (Int int)) =
--   do
--     (_, x86operand) <- operandToX86 operand
--     return ("", show int ++ x86operand)
-- operandToX86 (Padded operand@(GlobalEntry _) (Int int)) =
--   do
--     (_, x86operand) <- operandToX86 operand
--     return ("movl $" ++ x86operand ++ ", %edx", "(%edx)")
operandToX86 (GlobalEntry (VariableEntry name _)) = return ("", "_global_" ++ name)
--operandToX86 entry@(LocalEntry (ParameterEntry name (ArrayAnnotation _ _))) =
--  do
--    return ("", "FOUND ARRAY PARAMETER: " ++ name)
operandToX86 entry@(LocalEntry (ParameterEntry name _)) =
  do
    (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, x86) <- State.get
    let address = Map.lookup entry parameters
    if Maybe.isJust address
      then
        let (Just address') = address in return ("", show address' ++ "(%ebp)")
      else
        do
          State.put (arrays, globals, Map.insert entry parameterOffset parameters, parameterOffset + 4, locals, localOffset, data', bss, x86)
          return ("", show parameterOffset ++ "(%ebp)")
operandToX86 entry@(LocalEntry entry') =
  do
    (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, x86) <- State.get
    let address = Map.lookup entry locals
    if Maybe.isJust address
      then
        let (Just address') = address in return ("", show address' ++ "(%ebp)")
      else
        do
          State.put (arrays, globals, parameters, parameterOffset, Map.insert entry localOffset locals, localOffset - (4 * arrayLength entry'), data', bss, x86)
          return ("", show localOffset ++ "(%ebp)")
operandToX86 entry@(Temporary _) =
  do
    (arrays, globals, parameters, parameterOffset, locals, localOffset, data', bss, x86) <- State.get
    let address = Map.lookup entry locals
    if Maybe.isJust address
      then
        let (Just address') = address in return ("", show address' ++ "(%ebp)")
      else
        do
          State.put (arrays, globals, parameters, parameterOffset, Map.insert entry localOffset locals, localOffset - 4, data', bss, x86)
          return $ let address' = show localOffset ++ "(%ebp)" in ("", address')
operandToX86 operand = return ("", prettyPrint operand)

intermediatesToX86' :: [IntermediateCode] -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
intermediatesToX86' intermediates@(entry@(Label (GlobalEntry (FunctionEntry _ _ _))) : intermediates') =
  do
    let (function, rest) = (takeWhile notEntryPoint intermediates', dropWhile notEntryPoint intermediates')
    clearLocals
    clearParameters
    functionToX86 $ entry : function
    intermediatesToX86' rest
  where
    notEntryPoint (Global _) = False
    notEntryPoint (Label (GlobalEntry _)) = False
    notEntryPoint _ = True
intermediatesToX86' ((Global (GlobalEntry entry@(VariableEntry name annotation))) : rest) =
  do
    let qualifiedName = "_global_" ++ name
    --registerGlobal entry qualifiedName
    appendDataInline $ qualifiedName ++ ": .long 0"
    Monad.replicateM_ (arrayLength entry - 1) (appendDataInline ", 0")
    appendDataInline "\n"
    intermediatesToX86' rest
intermediatesToX86' [] = return ()

functionToX86 :: [IntermediateCode] -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
functionToX86 ((Label (GlobalEntry entry@(FunctionEntry _ name parameters))) : body) =
  do
    appendCode $ ".type " ++ name ++ ", @function"
    appendCode $ name ++ ":"
    appendIndented "pushl %ebp"
    appendIndented "movl %esp, %ebp"
    (_, _, _, _, _, _, _, _, x86) <- State.get
    let position = length (lines x86)
    mapM_ intermediateToX86 body
    insertFrame position

intermediateToX86 :: IntermediateCode -> State.State (Map.Map SymbolTableEntry Int, Map.Map SymbolTableEntry String, Map.Map Operand Int, Int, Map.Map Operand Int, Int, String, String, String) ()
intermediateToX86 (BinaryApplication Equal operand operand' operand'') =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl " ++ x86operand' ++ ", %eax"
    appendIndented $ "cmp " ++ x86operand'' ++ ", %eax"
    appendIndented "movl $0, %eax"
    appendIndented "movl %eax, %ebx"
    appendIndented "movl $1, %eax"
    appendIndented "cmove %eax, %ebx"
    appendIndented $ "movl %ebx, " ++ x86operand
intermediateToX86 (BinaryApplication NotEqual operand operand' operand'') =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl " ++ x86operand' ++ ", %eax"
    appendIndented $ "cmp " ++ x86operand'' ++ ", %eax"
    appendIndented "movl $0, %eax"
    appendIndented "movl %eax, %ebx"
    appendIndented "movl $1, %eax"
    appendIndented "cmovne %eax, %ebx"
    appendIndented $ "movl %ebx, " ++ x86operand
intermediateToX86 (BinaryApplication Less operand operand' operand'') =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl " ++ x86operand' ++ ", %eax"
    appendIndented $ "cmp " ++ x86operand'' ++ ", %eax"
    appendIndented "movl $0, %eax"
    appendIndented "movl %eax, %ebx"
    appendIndented "movl $1, %eax"
    appendIndented "cmovl %eax, %ebx"
    appendIndented $ "movl %ebx, " ++ x86operand
intermediateToX86 (BinaryApplication Greater operand operand' operand'') =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl " ++ x86operand'' ++ ", %eax"
    appendIndented $ "cmp " ++ x86operand' ++ ", %eax"
    appendIndented "movl $0, %eax"
    appendIndented "movl %eax, %ebx"
    appendIndented "movl $1, %eax"
    appendIndented "cmovl %eax, %ebx"
    appendIndented $ "movl %ebx, " ++ x86operand
intermediateToX86 (BinaryApplication Addition operand operand' operand'') =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl " ++ x86operand' ++ ", %eax"
    appendIndented $ "movl " ++ x86operand'' ++ ", %ebx"
    appendIndented $ "addl %ebx, %eax"
    appendIndented $ "movl %eax, " ++ x86operand
intermediateToX86 (BinaryApplication Subtraction operand operand' operand'') =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl " ++ x86operand' ++ ", %eax"
    appendIndented $ "movl " ++ x86operand'' ++ ", %ebx"
    appendIndented $ "subl %ebx, %eax"
    appendIndented $ "movl %eax, " ++ x86operand
intermediateToX86 (BinaryApplication Multiplication operand operand' operand'') =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl " ++ x86operand' ++ ", %eax"
    appendIndented $ "movl " ++ x86operand'' ++ ", %ebx"
    appendIndented $ "imull %ebx, %eax"
    appendIndented $ "movl %eax, " ++ x86operand
intermediateToX86 (BinaryApplication Division operand operand' operand'') =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented "movl $0, %edx"
    appendIndented $ "movl " ++ x86operand' ++ ", %eax"
    appendIndented $ "movl " ++ x86operand'' ++ ", %ebx"
    appendIndented $ "idivl %ebx, %eax"
    appendIndented $ "movl %eax, " ++ x86operand
intermediateToX86 (UnaryApplication AdditiveInverse operand operand') =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    appendIndented x86instruction
    appendIndented x86instruction'
    appendIndented $ "movl " ++ x86operand' ++ ", %eax"
    appendIndented $ "negl %eax"
    appendIndented $ "movl %eax, " ++ x86operand
intermediateToX86 (UnaryApplication Negation operand operand') =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    appendIndented x86instruction
    appendIndented x86instruction'
    appendIndented $ "movl " ++ x86operand' ++ ", %eax"
    appendIndented "cmp $0, %eax"
    appendIndented "movl $1, %eax"
    appendIndented "movl %eax, %ebx"
    appendIndented "movl $0, %eax"
    appendIndented "cmovne %eax, %ebx"
    appendIndented $ "movl %ebx, " ++ x86operand
intermediateToX86 (Assign operand operand') =
  do
    (_, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    appendIndented x86instruction'
    appendIndented $ "movl " ++ x86operand' ++ ", %ecx"
    appendIndented $ "movl %ecx, " ++ x86operand
intermediateToX86 (IndexedAccess operand (Padded operand'@(LocalEntry (VariableEntry _ _)) padding) operand'') =
  do
    (_, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "leal " ++ x86operand' ++ ", %eax"
    appendIndented $ "subl " ++ x86operand'' ++ ", %eax"
    appendIndented "movl (%eax), %ebx"
    appendIndented $ "movl %ebx, " ++ x86operand
intermediateToX86 (IndexedAccess operand (Padded operand'@(LocalEntry (ParameterEntry _ _)) padding) operand'') =
  do
    (_, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl " ++ x86operand' ++ ", %eax"
    appendIndented $ "subl " ++ x86operand'' ++ ", %eax"
    appendIndented "movl (%eax), %ebx"
    appendIndented $ "movl %ebx, " ++ x86operand
intermediateToX86 (IndexedAccess operand (Padded operand'@(GlobalEntry (VariableEntry _ _)) padding) operand'') =
  do
    (_, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl $" ++ x86operand' ++ ", %eax"
    appendIndented $ "subl " ++ x86operand'' ++ ", %eax"
    appendIndented "movl (%eax), %ebx"
    appendIndented $ "movl %ebx, " ++ x86operand
intermediateToX86 (IndexedAssign operand@(LocalEntry (VariableEntry _ _)) operand' operand'') =
  do
    (_, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "leal " ++ x86operand ++ ", %eax"
    appendIndented $ "subl " ++ x86operand' ++ ", %eax"
    appendIndented $ "movl " ++ x86operand'' ++ ", %ebx"
    appendIndented "movl %ebx, (%eax)"
intermediateToX86 (IndexedAssign operand@(LocalEntry (ParameterEntry _ _)) operand' operand'') =
  do
    (_, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl " ++ x86operand ++ ", %eax"
    appendIndented $ "subl " ++ x86operand' ++ ", %eax"
    appendIndented $ "movl " ++ x86operand'' ++ ", %ebx"
    appendIndented "movl %ebx, (%eax)"
intermediateToX86 (IndexedAssign operand@(GlobalEntry (VariableEntry _ _)) operand' operand'') =
  do
    (_, x86operand) <- operandToX86 operand
    (x86instruction', x86operand') <- operandToX86 operand'
    (x86instruction'', x86operand'') <- operandToX86 operand''
    appendIndented x86instruction'
    appendIndented x86instruction''
    appendIndented $ "movl $" ++ x86operand ++ ", %eax"
    appendIndented $ "subl " ++ x86operand' ++ ", %eax"
    appendIndented $ "movl " ++ x86operand'' ++ ", %ebx"
    appendIndented "movl %ebx, (%eax)"
intermediateToX86 (Push operand) =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    appendIndented x86instruction
    appendIndented $ handleArray operand x86operand
    appendIndented $ "pushl %eax"
  where
    handleArray (LocalEntry (VariableEntry _ (ArrayAnnotation _ _))) x86 = "leal " ++ x86 ++ ", %eax"
    handleArray _ x86 = "movl " ++ x86 ++ ", %eax"
intermediateToX86 (Pop operand@(LocalEntry (ParameterEntry name _))) =
  do
    operandToX86 operand
    return ()
intermediateToX86 (GotoFunction operand name size) =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    appendIndented x86instruction
    appendIndented $ "call " ++ name
    appendIndented $ "movl %eax, " ++ x86operand
intermediateToX86 (Label (Temporary name)) = appendCode $ name ++ ":"
intermediateToX86 (Goto (Temporary name)) = appendIndented $ "jmp " ++ name
intermediateToX86 (Return operand) =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    appendIndented x86instruction
    appendIndented $ "movl " ++ x86operand ++ ", %eax"
    appendIndented "movl %ebp, %esp"
    appendIndented "popl %ebp"
    appendIndented "ret"
intermediateToX86 (If operand (Temporary consequent)) =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    appendIndented x86instruction
    appendIndented $ "movl " ++ x86operand ++ ", %eax"
    appendIndented "cmp $0, %eax"
    appendIndented $ "jne " ++ consequent
intermediateToX86 (WriteInt operand) =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    appendIndented x86instruction
    appendIndented $ "pushl " ++ x86operand
    appendIndented "call print_int"
    appendIndented "addl $4, %esp"
intermediateToX86 (WriteChar operand) =
  do
    (x86instruction, x86operand) <- operandToX86 operand
    appendIndented x86instruction
    appendIndented $ "leal " ++ x86operand ++ ", %eax"
    appendIndented "pushl %eax"
    appendIndented "call print_char"
    appendIndented "addl $4, %esp"
    appendIndented "leal nl, %eax"
    appendIndented "pushl %eax"
    appendIndented "call print_char"
    appendIndented "addl $4, %esp"
intermediateToX86 (Read operand) =
  do
    (_, x86operand) <- operandToX86 operand
    appendIndented $ "call read_" ++ typeOf operand
    appendIndented $ "movl %eax, " ++ x86operand
  where
    typeOf (GlobalEntry (VariableEntry _ IntAnnotation)) = "int"
    typeOf (GlobalEntry (VariableEntry _ CharAnnotation)) = "char"
    typeOf (LocalEntry (VariableEntry _ IntAnnotation)) = "int"
    typeOf (LocalEntry (VariableEntry _ CharAnnotation)) = "char"
    typeOf (LocalEntry (ParameterEntry _ IntAnnotation)) = "int"
    typeOf (LocalEntry (ParameterEntry _ CharAnnotation)) = "char"

intermediateToX86 _ = return ()
