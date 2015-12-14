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
    where almostEqual e1 e2 = abs ((eval' e1) - (eval' e2)) < 0.001
          eval' e = eval e 3.14
