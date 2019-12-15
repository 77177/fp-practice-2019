module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Operation = Addition | Subtraction | Multiplication deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, op::Operation, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет


remove :: String -> String -> String
remove l r = [x | x <- l, y <- r, x == y]

(|+|) :: Term -> Term -> Term
(|+|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|+|) (Variable l) (Variable r) = Variable (l ++ r)
(|+|) l r = BinaryTerm l Addition r

(|-|) :: Term -> Term -> Term
(|-|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|-|) (Variable l) (Variable r) = Variable (remove l r)
(|-|) l r = BinaryTerm l Subtraction r

(|*|) :: Term -> Term -> Term
(|*|) (IntConstant l) (IntConstant r) = IntConstant (l * r)
(|*|) l r = BinaryTerm l Multiplication r

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

--Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
        case expression of
            BinaryTerm leftTerm operation rightTerm -> BinaryTerm (replaceVar (varName) (replacement) (leftTerm)) (operation) (replaceVar (varName) (replacement) (rightTerm))
            Variable v -> if(v == varName) then replacement else expression
            IntConstant i -> IntConstant i



-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term

evalBinaryTerm leftTerm operation rightTerm = case (evaluate leftTerm, operation, evaluate rightTerm ) of
                                 ((IntConstant (x)), (Addition), (IntConstant (y))) -> (leftTerm |+| rightTerm)
                                 ((IntConstant (x)), (Subtraction), (IntConstant (y))) -> leftTerm |-| rightTerm
                                 ((IntConstant (x)), (Multiplication), (IntConstant (y))) -> leftTerm |*| rightTerm
                                 _ -> BinaryTerm leftTerm operation rightTerm

evaluate expression = case expression of
        BinaryTerm leftTerm operation rightTerm -> evalBinaryTerm leftTerm operation rightTerm
        _-> expression