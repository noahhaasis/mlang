{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}

module Expr
  ( Literal (..),
    Identifier,
    Expr (..),
    lit,
    ref,
    app,
    fun,
    pattern Lit,
    pattern Ref,
    pattern App,
    pattern Fun,
    AnnotatedExpr (..),
    pattern AnnotLit,
    pattern AnnotRef,
    pattern AnnotApp,
    pattern AnnotFun,
    annotation,
  )
where

import Data.Functor.Foldable
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict as Map

type Identifier = String

data Literal
  = LNum Int
  | LBool Bool
  | LText String
  | LUnit
  deriving (Eq, Show)

data ExprF f
  = ELit Literal
  | ERef Identifier
  | EApp f (NonEmpty f)
  | EFun [Identifier] f
  deriving (Show, Functor, Foldable, Traversable)

type Expr = Fix ExprF

lit = Fix . ELit

ref = Fix . ERef

app f args = Fix $ EApp f args

fun params body = Fix (EFun params body)

pattern Lit l <- Fix (ELit l)

pattern Ref v <- Fix (ERef v)

pattern App f as <- Fix (EApp f as)

pattern Fun params body <- Fix (EFun params body)

newtype AnnotatedExpr a
  = AnnotatedExpr (a, ExprF (AnnotatedExpr a))
  deriving (Show, Functor, Foldable, Traversable)

annotation :: AnnotatedExpr a -> a
annotation (AnnotatedExpr (a, _)) = a

pattern AnnotLit annot l <- AnnotatedExpr (annot, ELit l)

pattern AnnotRef annot v <- AnnotatedExpr (annot, ERef v)

pattern AnnotApp annot f as <- AnnotatedExpr (annot, EApp f as)

pattern AnnotFun annot params body <- AnnotatedExpr (annot, EFun params body)
