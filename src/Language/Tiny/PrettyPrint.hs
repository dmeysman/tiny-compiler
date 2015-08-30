module Language.Tiny.PrettyPrint where

import qualified Data.List as List

class PrettyPrint a where

  prettyPrint :: a -> String

instance (PrettyPrint a) => PrettyPrint [a] where

  prettyPrint = mconcat . List.intersperse "\n" . fmap prettyPrint
