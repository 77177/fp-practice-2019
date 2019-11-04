module Task1_1 where

import Todo(todo)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет



(|+|) :: Term -> Term -> Term
(|+|) l r = case l of
        IntConstant _ -> IntConstant (intValue l + intValue r)
        Variable _ -> Variable ((varName l) ++ (varName r))
        BinaryTerm _ _ -> BinaryTerm ((|+|) (lhv l) (rhv l)) ((|+|) (lhv r) (rhv r))
(|-|) :: Term -> Term -> Term
(|-|) l r = case l of
        IntConstant _ -> IntConstant (intValue l - intValue r)
(|*|) :: Term -> Term -> Term
(|*|) l r = case l of
        IntConstant _ -> IntConstant (intValue l * intValue r)


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = todo

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = todo

