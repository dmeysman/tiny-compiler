module Language.Tiny.Synthesis.ValueNumbering where

import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Language.Tiny.Synthesis.Intermediate

numberValues :: [[IntermediateCode]] -> [[IntermediateCode]]
numberValues blocks = State.evalState (numberValues' blocks) Map.empty

numberValues' :: [[IntermediateCode]] -> State.State (Map.Map IntermediateCode Operand) [[IntermediateCode]]
numberValues' [] = return []
numberValues' (intermediates : blocks) =
  do
    eliminateds <- numberValuesBlock intermediates
    State.put Map.empty
    eliminatedBlocks <- numberValues' blocks
    return $ eliminateds : eliminatedBlocks

numberValuesBlock :: [IntermediateCode] -> State.State (Map.Map IntermediateCode Operand) [IntermediateCode]
numberValuesBlock [] = return []
numberValuesBlock (intermediate : intermediates) =
  do
    eliminated <- numberValuesCode intermediate
    eliminateds <- numberValuesBlock intermediates
    return $ eliminated : eliminateds

compute :: IntermediateCode -> State.State (Map.Map IntermediateCode Operand) ()
compute intermediate =
  do
    computeds <- State.get
    State.put $ Map.insert intermediate (result [intermediate]) computeds

uncompute :: Operand -> State.State (Map.Map IntermediateCode Operand) ()
uncompute operand =
  do
    computeds <- State.get
    State.put $ Map.filter (not . (== operand)) computeds

computed :: IntermediateCode -> State.State (Map.Map IntermediateCode Operand) (Maybe Operand)
computed (BinaryApplication operation _ operand operand') =
  do
    computeds <- State.get
    return . fmap snd . listToMaybe . filter match $ Map.assocs computeds
  where
    match ((BinaryApplication matchedOperation _ matchedOperand matchedOperand'), _) = matchedOperation == operation && matchedOperand == operand && matchedOperand' == operand'
    listToMaybe [] = Nothing
    listToMaybe (x : xs) = Just x
computed (UnaryApplication operation _ operand) =
  do
    computeds <- State.get
    return . fmap snd . listToMaybe . filter match $ Map.assocs computeds
  where
    match ((UnaryApplication matchedOperation _ matchedOperand), _) = matchedOperation == operation && matchedOperand == operand
    match _ = False
    listToMaybe [] = Nothing
    listToMaybe (x : xs) = Just x
computed intermediate = return Nothing

numberValuesCode :: IntermediateCode -> State.State (Map.Map IntermediateCode Operand) IntermediateCode
numberValuesCode intermediate@(BinaryApplication _ operand _ _) =
  do
    computed' <- computed intermediate
    if Maybe.isJust computed'
      then let (Just computed'') = computed' in return $ Assign operand computed''
      else
        do
          compute intermediate
          return intermediate
numberValuesCode intermediate@(UnaryApplication _ operand _) =
  do
    computed' <- computed intermediate
    if Maybe.isJust computed'
      then let (Just computed'') = computed' in return $ Assign operand computed''
      else
        do
          compute intermediate
          return intermediate
numberValuesCode intermediate@(Assign operand _) =
  do
    uncompute operand
    return intermediate
numberValuesCode intermediate@(IndexedAccess operand _ _) =
  do
    uncompute operand
    return intermediate
numberValuesCode intermediate = return intermediate
