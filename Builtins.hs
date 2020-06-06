module Builtins where

import Data.Map (Map)
import qualified Data.Map as Map
import Expr
import Type

builtinTypes :: Map Identifier Type
builtinTypes =
  Map.fromList
    [ ("+", TInt `TArrow` (TInt `TArrow` TInt)),
      ("-", TInt `TArrow` (TInt `TArrow` TInt))
    ] -- TODO: print builtin
