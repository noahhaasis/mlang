{-# LANGUAGE FlexibleContexts #-}

module Typeinfer where

import Control.Monad (replicateM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.State.Strict (MonadState, evalState, get, modify)
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

freshVar :: MonadState Int m => m TypeVar
freshVar = modify (+ 1) >> (show <$> get)

annotate :: Expr -> AnnotatedExpr TypeVar
annotate = flip evalState 0 . annotate'

annotate' ::
  MonadState Int m => Expr -> m (AnnotatedExpr TypeVar)
annotate' (Fix e) =
  curry AnnotatedExpr
    <$> freshVar
    <*> traverse annotate' e

type Constraints = Set (Type, Type)

type InferEnv = Map TypeVar Type

data AnnotationError = UnboundVar Identifier

-- TODO: Refactor using recursion schemes.
-- Maybe even combine this pass with the annotation pass,
-- since we have to generate fresh vars during constraint
-- generation anyways.
collectConstraints ::
  ( MonadReader InferEnv m,
    MonadError AnnotationError m,
    MonadState Int m
  ) =>
  AnnotatedExpr TypeVar ->
  m Constraints
collectConstraints (AnnotLit annot l) =
  return $ Set.singleton (TVar annot, groundType l)
collectConstraints (AnnotRef annot v) =
  ask >>= \env -> case Map.lookup v env of
    Just t -> return $ Set.singleton (TVar annot, t)
    Nothing -> throwError $ UnboundVar v
collectConstraints (AnnotApp annot f args) = do
  let argTypes = TVar . annotation <$> args
  returnType <- TVar <$> freshVar
  let functionType = foldr TArrow returnType argTypes
  return $
    Set.fromList
      [ (TVar annot, returnType),
        (TVar $ annotation f, functionType)
      ]
collectConstraints (AnnotFun annot params b) = do
  paramTypes <- replicateM (length params) (TVar <$> freshVar)
  let paramMap = Map.fromList $ zip params paramTypes
  cs <- local (Map.union paramMap) $ collectConstraints b
  let returnType = TVar $ annotation b
  let functionType = foldr TArrow returnType paramTypes
  return $
    Set.singleton (TVar annot, functionType)
      `Set.union` cs

groundType :: Literal -> Type
groundType (LNum _) = TInt
groundType (LBool _) = TBool
groundType LUnit = TUnit

type Substitution = Map TypeVar Type

data UnificationError = UnableToUnify Type Type

solveConstraints ::
  Constraints -> Either UnificationError Substitution
solveConstraints cs = case Set.minView cs of
  Just (c, cs') -> case c of
    (t, t') | t == t' -> solveConstraints cs'
    (TVar v, t) -> unifyWithTypeVar v t cs
    (t, TVar v) -> unifyWithTypeVar v t cs
    (TArrow t1 t2, TArrow t1' t2') ->
      solveConstraints
        (Set.fromList [(t1, t1'), (t2, t2')] `Set.union` cs')
    (t, t') -> Left $ UnableToUnify t t'
  Nothing -> return Map.empty
  where
    unifyWithTypeVar v t cs =
      Map.insert v t
        <$> solveConstraints (substituteInConstraints v t cs)
    substituteInConstraints v t =
      Set.map (fmap $ substituteInType v t)
    substituteInType v t (TVar v') | v == v' = t
    substituteInType v t (TArrow t1 t2) =
      TArrow
        (substituteInType v t t1)
        (substituteInType v t t2)
    substituteInType _ _ t' = t'

applySubstitution ::
  Substitution ->
  AnnotatedExpr TypeVar ->
  TypedExpr
applySubstitution s = fmap (\v -> Map.findWithDefault (TVar v) v s)
