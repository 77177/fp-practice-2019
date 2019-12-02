module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Operation = Addition | Subtraction | Multipication deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, op::Operation, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
remove :: String -> String -> String
remove l r = [x | x <- l, y <- r, x == y]


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
(|*|) l r = BinaryTerm l Multipication r

infixl 1 |+|
infixl 1 |-|
infixl 2 |*|

--Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
        case expression of
            IntConstant i -> IntConstant i
            BinaryTerm leftTerm operation rightTerm -> BinaryTerm (replaceVar (varName) (replacement) (lhv)) (op) (replaceVar (varName) (replacement) (rhv))
            Variable v -> if(v == varName) then replacement else expression


-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression =
    case expression of
        BinaryTerm leftTerm operation rightTerm ->
            case (evaluate lhv, op, evaluate rhv ) of
                (IntConstant lhv, Addition, IntConstant rhv) -> IntConstant (lhv |+| rhv)
                (IntConstant lhv, Subtraction, IntConstant rhv) -> IntConstant (lhv |-| rhv)
                (IntConstant lhv, Multipication, IntConstant rhv) -> IntConstant (lhv |*| rhv)
                _ -> BinaryTerm leftTerm operation rightTerm
        _-> expression