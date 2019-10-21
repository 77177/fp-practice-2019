module Task1_1 where

import Todo(todo)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
remove :: String -> String -> String
remove l r = [x | x <- l, y <- r, x == y]

(|+|) :: Term -> Term -> Term
(|+|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|+|) (Variable l) (Variable r) = Variable (l ++ r)
(|+|) (BinaryTerm l m) (BinaryTerm r n) = BinaryTerm ((|+|) (l) (r))((|+|) (m) (n))

(|-|) :: Term -> Term -> Term
(|-|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|-|) (Variable l) (Variable r) = Variable (remove l r)
(|-|) (BinaryTerm l m) (BinaryTerm r n) = BinaryTerm ((|-|) (l) (r))((|-|) (m) (n))

(|*|) :: Term -> Term -> Term
(|*|) (IntConstant l) (IntConstant r) = IntConstant (l * r)


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = todo

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = todo

