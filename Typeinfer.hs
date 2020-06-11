{-# LANGUAGE FlexibleContexts #-}

module Typeinfer where

import Builtins
import Control.Monad (replicateM)
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, evalState, evalStateT, get, modify)
import Data.Functor.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Expr
import Type

infer :: Expr -> Either InferError TypedExpr
infer e = do
  let annotExpr = annotate e
  cs <- runCollectConstraintsM builtinTypes $ collectConstraints annotExpr
  subs <- map generalize <$> solveConstraints cs
  return $ applySubstitution subs annotExpr

annotate :: Expr -> AnnotatedExpr TypeVar
annotate = flip evalState 0 . annotate'

annotate' ::
  MonadState Int m => Expr -> m (AnnotatedExpr TypeVar)
annotate' (Expr e) =
  curry AnnotatedExpr
    <$> freshVar
    <*> traverse annotate' e

type Constraints = Set (Type, Type)

data InferError
  = UnboundVar Identifier
  | UnableToUnify Type Type
  deriving (Show)

runCollectConstraintsM ::
  TypeEnv ->
  StateT Int (ReaderT TypeEnv (Except InferError)) a ->
  Either InferError a
runCollectConstraintsM env =
  runExcept
    . flip runReaderT env
    . flip evalStateT 0

-- TODO: Refactor using recursion schemes.
-- Maybe even combine this pass with the annotation pass,
-- since we have to generate fresh vars during constraint
-- generation anyways.
collectConstraints ::
  ( MonadReader TypeEnv m,
    MonadError InferError m,
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
  argConstraints <- traverse collectConstraints args
  functionConstraints <- collectConstraints f
  let argTypes = TVar . annotation <$> args
  let functionType = foldr TArrow (TVar annot) argTypes
  return $
    Set.singleton
      (TVar $ annotation f, functionType)
      `Set.union` Set.unions (NE.toList argConstraints)
      `Set.union` functionConstraints
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

solveConstraints ::
  Constraints -> Either InferError Substitution
solveConstraints cs = case Set.minView cs of
  Just (c, cs') -> case c of
    (t, t') | t == t' -> solveConstraints cs'
    (TVar v, t) | v `notElem` fvs t -> unifyWithTypeVar v t cs'
    (t, TVar v) | v `notElem` fvs t -> unifyWithTypeVar v t cs'
    (TArrow t1 t2, TArrow t1' t2') ->
      solveConstraints
        (Set.fromList [(t1, t1'), (t2, t2')] `Set.union` cs')
    (t, t') -> Left $ UnableToUnify t t'
  Nothing -> return Map.empty
  where
    unifyWithTypeVar v t cs =
      Map.insert v t
        <$> solveConstraints (substituteInConstraints v t cs)
    substituteInConstraints v t = Set.map subsInEquation
      where
        subsInEquation (t1, t2) = (substituteInType v t t1, substituteInType v t t2)
    substituteInType v t (TVar v') | v == v' = t
    substituteInType v t (TArrow t1 t2) =
      TArrow
        (substituteInType v t t1)
        (substituteInType v t t2)
    substituteInType _ _ t' = t'

fvs :: Type -> Set Identifier
fvs (TVar v) = Set.singleton v
fvs (TArrow t1 t2) = Set.union (fvs t2) (fvs t1)
fvs _ = Set.empty

generalize :: Type -> Scheme
generalize t = case Set.toList $ fvs t of
  [] -> Monotype t
  vars -> Forall vars t

applySubstitution ::
  Substitution ->
  AnnotatedExpr TypeVar ->
  TypedExpr
applySubstitution s = fmap (\v -> Map.findWithDefault (TVar v) v s)
