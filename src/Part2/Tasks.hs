module Part2.Tasks where

import Util(notImplementedYet)
import Lambdas (times)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 7 |+|
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 7 |-|
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 8 |*|
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (IntConstant x) = IntConstant x
replaceVar varName replacement (Variable x) | x == varName = replacement
                                            | otherwise = Variable x
replaceVar varName replacement (BinaryTerm op t1 t2) = BinaryTerm op (replaceVar varName replacement t1) (replaceVar varName replacement t2)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant x) = IntConstant x
evaluate (Variable x) = Variable x
evaluate (BinaryTerm op t1 t2) =
   case (op, evaluate t1, evaluate t2) of
      (Plus, IntConstant a, IntConstant b) -> IntConstant (a + b)
      (Minus, IntConstant a, IntConstant b) -> IntConstant (a - b)
      (Times, IntConstant a, IntConstant b) -> IntConstant (a * b)
      (Plus, t1, t2) -> BinaryTerm Plus t1 t2 
      (Minus, t1, t2) -> BinaryTerm Minus t1 t2
      (Times, t1, t2) -> BinaryTerm Times t1 t2
