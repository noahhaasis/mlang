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
  deriving (Eq, Ord, Show)

type TypedExpr = AnnotatedExpr Type

type TypeVar = String

freshVar :: State Int TypeVar
freshVar = modify (+ 1) >> (show <$> get)

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

data UnificationError = UnableToUnify Type Type

solveConstraints ::
  Constraints -> Either UnificationError Substitution
solveConstraints cs = case Set.minView cs of
  Just (c, cs') -> case c of
    (t, t') | t == t' -> solveConstraints cs'
    (TVar v, t) ->
      Map.insert v t
        <$> solveConstraints (substituteInConstraints v t cs')
    (t, TVar v) -> undefined
    (t, t') -> Left $ UnableToUnify t t'
  Nothing -> return Map.empty
  where
    substituteInConstraints v t =
      Set.map (fmap $ substituteInType v t)
    substituteInType v t (TVar v') | v == v' = t
    substituteInType v t (TArrow t1 t2) =
      TArrow
        (substituteInType v t t1)
        (substituteInType v t t2)
    substituteInType _ _ t' = t'

-- TODO: Return the type var instead of failing when
--       the type var isn't in the substitution
applySubstitution ::
  Substitution ->
  AnnotatedExpr TypeVar ->
  Maybe TypedExpr
applySubstitution s = traverse (`Map.lookup` s)
