module Parser where

import Expr

data ParseError = FailedToParser -- TODO

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (mapFst f) . p)
    where
      mapFst f (a, b) = (f a, b)

instance Applicative Parser where
  pure = undefined
  pf <*> pa = undefined -- TODO

instance Monad Parser where
  -- (>>=) :: m a -> (a -> m b) -> m b
  (Parser ma) >>= f = Parser $ \s ->
    undefined $ ma s -- TODO

exprP :: Parser Expr
exprP = undefined

parse :: String -> Either ParseError Expr
parse = undefined
