{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict as Map
import Expr (Expr)
import qualified Expr as E

data RuntimeExpr
  = RLit E.Literal
  | RApp RuntimeExpr (NonEmpty RuntimeExpr)
  | RFun Function
  | RRef E.Identifier
  | RBuiltin Builtin
  | RClosure Env Function
  deriving (Show)

data Function = Function [E.Identifier] RuntimeExpr
  deriving (Show)

type Env = Map E.Identifier RuntimeExpr

data Builtin
  = Builtin
      { name :: E.Identifier,
        evalBuiltin :: [RuntimeExpr] -> EvalM RuntimeExpr
      }

instance Show Builtin where
  show (Builtin name _) = "{builtin: `" ++ name ++ "`}"

builtins :: Map.Map E.Identifier Builtin
builtins =
  Map.fromList
    [ ("+", Builtin "+" plusB),
      ("-", Builtin "-" minusB),
      ("print", Builtin "print" printB)
    ]

plusB :: [RuntimeExpr] -> EvalM RuntimeExpr
plusB [RLit (E.LNum a), RLit (E.LNum b)] =
  return $ RLit $ E.LNum (a + b)
plusB _args = throwE undefined

minusB :: [RuntimeExpr] -> EvalM RuntimeExpr
minusB [RLit (E.LNum a), RLit (E.LNum b)] =
  return $ RLit $ E.LNum (a - b)
minusB _args = throwE undefined

printB :: [RuntimeExpr] -> EvalM RuntimeExpr
printB [RLit (E.LText t)] = liftIO (RLit E.LUnit <$ putStrLn t)
printB _args = throwE undefined

inject :: E.Expr -> RuntimeExpr
inject (E.Lit l) = RLit l
inject (E.Ref i) = RRef i
inject (E.App f as) = RApp (inject f) (inject <$> as)
inject (E.Fun params body) =
  RFun $ Function params $ inject body

data EvalError
  = UnboundVar E.Identifier
  | IllegalEmptyApplication
  | ExpectedFunctionButGot RuntimeExpr
  | ParityMismatch
      { numberOfParams :: Int,
        numberOfArgs :: Int
      }
  deriving (Show)

type EvalM = ExceptT EvalError IO

runEvalM :: EvalM a -> IO (Either EvalError a)
runEvalM = runExceptT

eval :: Expr -> EvalM RuntimeExpr
eval = eval' Map.empty . inject

eval' :: Env -> RuntimeExpr -> EvalM RuntimeExpr
eval' e (RLit l) = return $ RLit l
eval' e (RApp f args) = eval' e f >>= \case
  (RClosure e' (Function params body)) -> do
    when (length params /= length args)
      $ throwE
      $ ParityMismatch (length params) (length args)
    args' <- mapM (eval' e) args
    let newEnv = Map.union (Map.fromList (zip params $ NE.toList args')) e'
    eval' newEnv body
  (RBuiltin (Builtin name evalBuiltin)) -> do
    args' <- mapM (eval' e) args
    evalBuiltin $ NE.toList args'
  expr -> throwE $ ExpectedFunctionButGot expr
eval' e (RFun f) = return $ RClosure e f
eval' e (RRef v) = case Map.lookup v e of
  Just expr -> return expr
  Nothing -> case Map.lookup v builtins of
    Just b -> return $ RBuiltin b
    Nothing -> throwE $ UnboundVar v
