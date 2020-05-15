module Typeinfer where

import Control.Monad.State.Lazy (State, evalState, get, modify)
import Data.Functor.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Expr

data Type
  = TInt
  | TBool
  | TUnit
  | TArrow Type Type
  | TVar TypeVar

type TypedExpr = AnnotatedExpr Type

type TypeVar = String

freshVar :: State Int TypeVar
freshVar = modify (+1) >> (show <$> get)

annotate :: Expr -> AnnotatedExpr TypeVar
annotate = flip evalState 0 . annotate'

annotate' :: Expr -> State Int (AnnotatedExpr TypeVar)
annotate' (Fix e) =
  curry AnnotatedExpr 
    <$> freshVar 
    <*> traverse annotate' e

type Constraints = (Set (Type, Type))

collectConstraints :: AnnotatedExpr TypeVar -> Constraints
collectConstraints = undefined

type Substitution = Map TypeVar Type

solveConstraints :: Constraints -> Substitution
solveConstraints = undefined

applySubstitution
  :: Substitution
  -> AnnotatedExpr TypeVar
  -> Maybe TypedExpr
applySubstitution s = traverse (`Map.lookup` s)

