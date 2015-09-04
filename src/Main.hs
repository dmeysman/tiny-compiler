module Main where

  import System.Environment (getArgs)
  import Control.Monad.State (runState)

  import Language.Tiny.Analysis.Lexical (alexScanTokens)
  import Language.Tiny.Analysis.Syntactic (happyParseTokens)
  import Language.Tiny.Analysis.ConstantFolding (foldConstantsProgram)
  import Language.Tiny.Analysis.DeadCodeElimination (eliminateDeadCodeProgram)
  import Language.Tiny.PrettyPrint (prettyPrint)
  -- import Language.Tiny.Synthesis.ConstantPropagation (propagateConstants)
  import Language.Tiny.Synthesis.Intermediate (programToIntermediate)
  import Language.Tiny.Synthesis.ValueNumbering (numberValues)
  import Language.Tiny.Synthesis.X86 (intermediatesToX86)

  main :: IO ()
  main =
    do
      (path : _) <- getArgs
      readInt <- readFile "lib/read_int.s"
      printInt <- readFile "lib/print_int.s"
      readChar <- readFile "lib/read_char.s"
      printChar <- readFile "lib/print_char.s"
      text <- readFile path
      let (tree, (_, table)) = runState (happyParseTokens (alexScanTokens text)) ([], [])
      -- print table
      -- mapM_ (putStrLn . (++ "\n") . prettyPrint) (propagateConstants (numberValues (programToIntermediate (eliminateDeadCodeProgram (foldConstantsProgram tree)) table)))
      putStrLn $ readInt ++ printInt ++ readChar ++ printChar ++ (intermediatesToX86 (numberValues (programToIntermediate (foldConstantsProgram tree) table)))
