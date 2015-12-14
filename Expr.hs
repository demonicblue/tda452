module Expr where

import Parsing
import Data.Char
import Data.Maybe
import Control.Monad

---- A ----
data Expr =
    Num Double
  | Var
  | Op OpType Expr Expr
  | Fun FunType Expr

data OpType  = Mul | Add
data FunType = Sin | Cos

---- B ----
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr Var = "x"

showExpr (Op Mul e1 e2) = showP e1 ++ "*" ++ showP e2
    where showP e@(Op Add _ _) = showExprP e
          showP  e              = showExpr  e

showExpr (Op t e1 e2) = showExpr e1 ++ showOp t ++ showExpr e2

showExpr (Fun f e) = showFun f ++ showP e
    where
          showP e@(Op _ _ _) = showExprP e
          showP e            = showExpr  e

showExprP e = "(" ++ showExpr e ++ ")"

showOp :: OpType -> String
showOp Mul = "*"
showOp Add = " + "

showFun :: FunType -> String
showFun Sin = "sin "
showFun Cos = "cos "

instance Show Expr where
  show = showExpr

instance Show FunType where
  show = showFun

instance Show OpType where
  show = showOp

---- C ----
eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval (Var) x   = x
eval (Op Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Op Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Fun Sin e) x = sin (eval e x)
eval (Fun Cos e) x = cos (eval e x)

---- D ---- Henrik + Matthias
readExpr :: String -> Maybe Expr
readExpr = undefined

---- F ---- Henrik
simplify :: Expr -> Expr
simplify = undefined

---- G ---- Matthias
differentiate :: Expr -> Expr
differentiate (Num _) = Num 0.0
differentiate (Var)   = Num 1.0
differentiate (Op Add e1 e2) = (Op Add e1' e2')
    where e1' = differentiate e1
          e2' = differentiate e2
differentiate (Op Mul e1 e2) = (Op Add (Op Mul e1' e2) (Op Mul e1 e2'))
    where e1' = differentiate e1
          e2' = differentiate e2

differentiate (Fun Sin e) = Fun Cos e
-- Cos(x)' = - Sin(x) = Sin (x + Pi)
differentiate (Fun Cos e) = Fun Sin (Op Add e (Num 3.14159265358979323))
