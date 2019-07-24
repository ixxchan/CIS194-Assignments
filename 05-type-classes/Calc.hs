{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where
import ExprT
import Parser
import StackVM
import qualified Data.Map as M

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit x    ) = x
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-- Exercise 3
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

-- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

-- Exercise 5
instance Expr Program where
    lit x = [PushI x]
    add s1 s2 = s1 ++ s2 ++ [StackVM.Add]
    mul s1 s2 = s1 ++ s2 ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
--    deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    lit = Calc.Lit
    add = Calc.Add
    mul = Calc.Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup 

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x _ = Just x
    add f1 f2 m = (+) <$> f1 m <*> f2 m
    mul f1 f2 m = (*) <$> f1 m <*> f2 m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs