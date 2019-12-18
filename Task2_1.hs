module Task2_1 where
import Prelude hiding (lookup)

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Empty | Node (Integer, v) (TreeMap v) (TreeMap v) deriving (Show, Eq, Read)



testTree = Node (100, "100") (Node (50, "500")(Empty)(Empty)) (Node (150, "150") (Empty)(Empty))


-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Empty

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Empty key = False
contains (Node (key, value) leftTree rightTree) k =
                                                    if k == key then True else
                                                    if k < key then contains leftTree k else contains rightTree k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup key Empty = error "The value with this key doesnt exist"
lookup k (Node (key, value) leftTree rightTree) =
                                                    if k == key then value else
                                                    if k < key then lookup k leftTree else lookup k rightTree


-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (key, value) Empty = Node (key, value) Empty Empty
insert (k, v) (Node (key, value) leftTree rightTree) =
                                                    if k == key then Node (k, v) leftTree rightTree else
                                                    if k < key then Node (key, value) (insert (k,v) leftTree) rightTree else Node (key, value) leftTree (insert (k,v) rightTree)


-- Удаление элемента по ключу
findMin :: TreeMap v -> (Integer, v)
findMin (Node (key, value) Empty right) = (key, value)
findMin (Node (key, value) left right) = findMin left

testament :: TreeMap v -> TreeMap v -> TreeMap v
testament left (Node (rkey, rvalue) Empty rright) = Node (rkey, rvalue) left rright
testament left (Node (rkey, rvalue) rleft rright) =
                                    let min = findMin rleft in
                                    Node min left (insert min rright)

remove :: Integer -> TreeMap v -> TreeMap v
remove _ Empty = error "A value with thsi key doesn't exist"
remove k (Node (key, value) leftTree rightTree)
                                              | k < key = remove k leftTree
                                              | k > key = remove k rightTree
                                              | k == key = case (leftTree, rightTree) of
                                                                                        (Empty, Empty) -> Empty
                                                                                        (Empty, rightTree) -> rightTree
                                                                                        (leftTree, Empty) -> leftTree
                                                                                        (leftTree, rightTree) -> testament leftTree rightTree

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i Empty = error "Could not find"
nearestLE i (Node (k, v) l r) | k == i = (k, v)
                              | k > i = nearestLE i l
                              | k < i = case(r) of
                                        Node (k, v) left _  | k == i -> (k, v)
                                                            | k < i -> nearestLE i r
                                                            | k > i -> case (left) of
                                                                 Empty -> (k, v)
                                                                 otherwise -> nearestLE i left
                                        otherwise -> (k, v)

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v

treeFromList lst = case lst of
                               [] -> Empty
                               _ -> foldr insert Empty lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree tree = case tree of
                                Empty -> []
                                (Node (key, value) leftTree rightTree) -> (key, value) : listFromTree (remove key (Node (key, value) leftTree rightTree))

-- Поиск k-той порядковой статистики дерева
tSize :: TreeMap v -> Integer
tSize Empty = 0
tSize (Node _ left right ) = (tSize left) + 1 + (tSize right)

kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = case (i, t) of
                            (_, Empty) -> error "The tree is empty"
                            (i, (Node (key, value) left right )) -> if (tSize left == i) then (key, value) else
                                                                   if (tSize left > i) then (kMean i left) else (kMean (i - (tSize left) - 1) right)


-- kMean _ Empty = error "Tree is empty"
-- kMean i (Node (key, value) left right ) | tSize left == i = (key, value)
--                                         | tSize left > i  = kMean i left
--                                         | otherwise     = kMean (i - (tSize left) - 1) right
