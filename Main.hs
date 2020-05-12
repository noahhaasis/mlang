module Main where

import Eval
import Expr
import Parser

programS = "(+ 1 2)"
program = EApp [(ERef "+"), (ELit $ LNum 1), (ELit $ LNum 2)]

program2S = "(- 10 (+ 1 2))"
program2 = EApp [(ERef "-"), (ELit $ LNum 10), program]

main :: IO ()
main = pure ()
