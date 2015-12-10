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
showExpr (Op t e1 e2) = showExpr e1 ++ showOp t ++ showExpr e2
-- TODO: Fix Parantheses 

showExpr (Fun f e) = showFun f ++ showP e
    where showP e@(Num _) = showExpr e
          showP e@(Var)   = showExpr e
          showP e         = "(" ++ showExpr e ++ ")"

showOp :: OpType -> String
showOp Mul = "*"
showOp Add = " + "

showFun :: FunType -> String
showFun Sin = "sin "
showFun Cos = "cos "

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

---- E ---- Matthias
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr = undefined

-- Henrik
arbExpr :: Int -> Gen Expr
arbExpr = undefined

instance Arbitrary Expr where
  arbitrary = sized arbExpr

---- F ---- Henrik
simplify :: Expr -> Expr
simplify = undefined

---- G ---- Matthias
differentiate :: Expr -> Expr
differentiate = undefined
