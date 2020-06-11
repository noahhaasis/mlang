{-# LANGUAGE FlexibleContexts #-}

module Type where

import Control.Monad.State.Strict (MonadState, StateT, evalState, evalStateT, get, modify)
import Data.Map (Map)
import Expr

data Scheme = Monotype Type | Forall [Identifier] Type

data Type
  = TInt
  | TBool
  | TUnit
  | TArrow Type Type
  | TVar TypeVar
  deriving (Eq, Ord)

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show TUnit = "()"
  show (TVar v) = v
  show (TArrow a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

type TypeEnv = Map Identifier Type

type TypedExpr = AnnotatedExpr Scheme

type TypeVar = String

freshVar :: MonadState Int m => m TypeVar
freshVar = do
  let chars = ['a', 'b', 'c', 'd']
  c <- get
  modify (+ 1)
  return $ (chars !! (c `mod` length chars)) : show c
