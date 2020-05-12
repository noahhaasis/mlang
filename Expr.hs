module Expr where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict as Map

type Identifier = String

data Expr
  = ELit Literal
  | ERef Identifier
  | EApp [Expr]
  | EFun [Identifier] Expr
  deriving Show

data Literal
  = LNum  Int
  | LBool Bool
  | LText String
  | LUnit
  deriving (Eq, Show)
