module Main where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Eval
import Expr
import Parser
import Typeinfer

programS = "(+ 1 2)"

program = app (ref "+") $ NE.fromList [lit $ LNum 1, lit $ LNum 2]

program2S = "(- 10 (+ 1 2))"

program2 = app (ref "-") $ NE.fromList [lit $ LNum 10, program]

main :: IO ()
main = pure ()
