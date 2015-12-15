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
readExpr s = do
             r <- parse parseExpr s
             return $ fst r

{-
<expr> ::= <term> | <term> "+" <expr>
<term> ::= <factor> | <factor> "*" <term>
<factor> ::= "(" <expr> ")" | <number> -}

parseExpr :: Parser Expr
parseExpr = foldr1 (Op Add) `fmap` chain term (parseOp '+')

term :: Parser Expr
term = foldr1 (Op Mul) `fmap` chain factor (parseOp '*')

parseOp :: Char -> Parser String
parseOp c = do 
              spaces
              string [c]
              spaces

factor :: Parser Expr
factor = parseP +++
         parseFun Sin "sin" +++
         parseFun Cos "cos" +++
         parseVar +++
         parseNum

parseP :: Parser Expr
parseP = do
           char '('
           e <- parseExpr
           char ')'
           return $ e

parseFun :: FunType -> String -> Parser Expr
parseFun f s = do
               string s
               spaces
               e <- factor
               return $ Fun f e

parseVar :: Parser Expr
parseVar = do
           char 'x'
           return Var

parseNum :: Parser Expr
parseNum = do
           d <- readsP :: Parser Double
           return $ Num d

string :: String -> Parser String
string s = sequence $ fmap (char) s

spaces :: Parser String
spaces = zeroOrMore (sat isSpace)

---- F ---- Henrik
simplify :: Expr -> Expr
simplify (Op t e1 e2)
        = simplify' (Op t   (simplify e1) (simplify e2))
simplify (Fun f e1)
        = simplify' (Fun f  (simplify e1))
simplify e = e

simplify' :: Expr -> Expr
simplify' (Op Add (Num n1) (Num n2))     = Num (n1 + n2)
simplify' e0@(Op Add (Num n) e) 
        | n == 0    = simplify e
        | otherwise = e0
simplify' e0@(Op Add e (Num n))
        | n == 0    = simplify e
        | otherwise = e0
simplify' (Op Mul (Num n1) (Num n2))     = Num (n1 * n2)
simplify' e0@(Op Mul (Num n) e) 
        | n == 0    = Num 0
        | n == 1    = simplify e
        | otherwise = e0
simplify' e0@(Op Mul e (Num n))
        | n == 0    = Num 0
        | n == 1    = simplify e
        | otherwise = e0
simplify' e = e

---- G ---- Matthias
differentiate :: Expr -> Expr
differentiate = simplify . differentiate'

differentiate' :: Expr -> Expr
differentiate' (Num _) = Num 0.0
differentiate' (Var)   = Num 1.0
differentiate' (Op Add e1 e2) = (Op Add e1' e2')
    where e1' = differentiate e1
          e2' = differentiate e2
differentiate' (Op Mul e1 e2) = (Op Add (Op Mul e1' e2) (Op Mul e1 e2'))
    where e1' = differentiate e1
          e2' = differentiate e2

differentiate' (Fun Sin e) = Fun Cos e
differentiate' (Fun Cos e) = (Op Mul (Num (-1)) (Fun Sin e))
