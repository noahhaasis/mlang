{-# LANGUAGE LambdaCase #-}
module Eval where

import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad (when)
import Data.Map.Strict as Map

import Expr (Expr)
import qualified Expr as E

data RuntimeExpr
  = RLit E.Literal
  | RApp [RuntimeExpr]
  | RFun Function
  | RRef E.Identifier
  | RBuiltin Builtin
  | RClosure Env Function
  deriving Show

data Function = Function [E.Identifier] RuntimeExpr
  deriving Show

data Builtin =
  Builtin 
    { name :: E.Identifier
    , paramCount :: Int -- TODO: Remove later and add EvalM to eval Builtin
    , evalBuiltin :: ([RuntimeExpr] -> RuntimeExpr)
    }

type Env = Map E.Identifier RuntimeExpr

instance Show Builtin where
  show (Builtin name _ _) = "{builtin: `" ++ name ++ "`}"

inject :: E.Expr -> RuntimeExpr
inject (E.ELit l) = RLit l
inject (E.ERef i) = RRef i
inject (E.EApp es) = RApp (inject <$> es)
inject (E.EFun params body) =
    RFun $ Function params $ inject body

data EvalError
  = UnboundVar E.Identifier
  | IllegalEmptyApplication
  | ExpectedFunctionButGot RuntimeExpr
  | ParityMismatch
    { numberOfParams :: Int
    , numberOfArgs :: Int
    }
  deriving (Show)

plusB :: [RuntimeExpr] -> RuntimeExpr
plusB [RLit (E.LNum a), RLit (E.LNum b)] = RLit $ E.LNum (a + b)

minusB :: [RuntimeExpr] -> RuntimeExpr
minusB [RLit (E.LNum a), RLit (E.LNum b)] = RLit $ E.LNum (a - b)

printB :: [RuntimeExpr] -> RuntimeExpr
printB [RLit (E.LText t)] = undefined -- TODO

builtins :: Map.Map E.Identifier Builtin
builtins = Map.fromList
  [ ("+", Builtin "+" 2 plusB)
  , ("-", Builtin "-" 2 minusB)
  ]

type EvalM = ExceptT EvalError IO

runEvalM :: EvalM a -> IO (Either EvalError a)
runEvalM = runExceptT

eval :: Expr -> EvalM RuntimeExpr
eval = eval' Map.empty . inject

eval' :: Env -> RuntimeExpr -> EvalM RuntimeExpr
eval' e (RLit l) = return $ RLit l
eval' e (RApp (f:args)) = eval' e f >>= \case
  (RClosure e' (Function params body)) -> do
    when (length params /= length args)
      $ throwE $ ParityMismatch (length params) (length args)
    args' <- mapM (eval' e) args
    let newEnv = Map.union (Map.fromList (zip params args')) e'
    eval' newEnv body
  (RBuiltin (Builtin name paramCount evalBuiltin)) -> do
    when (paramCount /= length args)
      $ throwE $ ParityMismatch paramCount (length args)
    args' <- mapM (eval' e) args
    return $ evalBuiltin args'
  expr -> throwE $ ExpectedFunctionButGot expr
eval' e (RApp []) = throwE IllegalEmptyApplication
eval' e (RFun f) = return $ RClosure e f
eval' e (RRef v) = case Map.lookup v e of
  Just expr -> return expr
  Nothing -> case Map.lookup v builtins of
    Just b -> return $ RBuiltin b
    Nothing -> throwE $ UnboundVar v

