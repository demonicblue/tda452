import Test.QuickCheck
import Data.Maybe
import Expr

-- Henrik
arbExpr :: Int -> Gen Expr
arbExpr n   | n == 1      = oneof 
    [
        return Var,
        do  r <- choose (0.0,100.0)
            return $ Num r
    ] 
            | otherwise   = oneof 
    [
        return Var,
        do  r <- choose (0.0,100.0)
            return $ Num r,
        do  o <- opType
            e1 <- arbExpr (n-1)
            e2 <- arbExpr (n-1)
            return (Op o e1 e2),
        do  f <- funType
            e <- arbExpr (n-1)
            return (Fun f e)
    ]

opType :: Gen OpType
opType = elements [Mul, Add]

funType :: Gen FunType
funType = elements [Sin, Cos]


instance Arbitrary Expr where
  arbitrary = sized arbExpr

---- E ---- Matthias
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = e `almostEqual` fromJust (readExpr (showExpr e))

almostEqual :: Expr -> Expr -> Bool
almostEqual e1 e2   = abs ((eval' e1) - (eval' e2)) < 0.001
    where
        eval' e = eval e 3.14

---- F ---- Henrik
prop_EvalSimplify :: Expr -> Bool
prop_EvalSimplify e = e `almostEqual` simplify e

one = Num 1
two = Num 2
e1 = Op Add one one
e2 = Op Mul one one
e3 = Op Mul one (Fun Cos Var)
e4 = Op Mul (Num 42) (Fun Cos Var)
e5 = Op Mul (Num 0) (Fun Cos Var)
e6 = Op Add e1 e1
e7 = Op Add (Fun Cos Var) e6