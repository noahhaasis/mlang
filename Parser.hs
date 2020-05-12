module Parser where

import Expr

data ParseError = FailedToParser -- TODO 

parse :: String -> Either ParseError Expr
parse = undefined

