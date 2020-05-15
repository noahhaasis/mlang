{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Expr
  ( Literal(..)
  , Identifier
  , Expr(..)
  , lit
  , ref
  , app
  , fun
  , pattern Lit
  , pattern Ref
  , pattern App
  , pattern Fun
  , AnnotatedExpr(..)
  )
  where

import Data.Functor.Foldable
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict as Map

type Identifier = String

data Literal
  = LNum  Int
  | LBool Bool
  | LText String
  | LUnit
  deriving (Eq, Show)

data ExprF f
  = ELit Literal
  | ERef Identifier
  | EApp [f]
  | EFun [Identifier] f
  deriving (Show, Functor, Foldable, Traversable)

type Expr = Fix ExprF

newtype AnnotatedExpr a =
    AnnotatedExpr (a, ExprF (AnnotatedExpr a))
    deriving (Show)

lit :: Literal -> Expr
lit = Fix . ELit

ref :: Identifier -> Expr
ref = Fix . ERef

app :: [Expr] -> Expr
app = Fix . EApp

fun :: [Identifier] -> Expr -> Expr
fun params body = Fix (EFun params body)

pattern Lit l <- Fix (ELit l)
pattern Ref v <- Fix (ERef v)
pattern App as <- Fix (EApp as)
pattern Fun params body <- Fix (EFun params body)

