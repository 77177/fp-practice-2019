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
remove :: Integer -> TreeMap v -> TreeMap v
remove k t = todo


-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)

findNearest i (Node (key, value) left right)
                                        | key == i = (key, value)
                                        | key > i = nearestLE i left
                                        | key < i = case right of
                                                                 Node (key, value) left right
                                                                    | i == key -> (key, value)
                                                                    | i /= key -> nearestLE i right
                                                                 Empty -> (key, value)

nearestLE i t = case t of
                        Empty -> error "element doesn't exist"
                        Node (key, value) (left) (right) -> (findNearest i t)


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
treeSize :: TreeMap v -> Integer
treeSize Empty = 0
treeSize t = case t of
                      Empty -> 0
                      (Node _ left right ) -> (treeSize left) + 1 + (treeSize right)

kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = case (i, t) of
                            (_, Empty) -> error "The tree is empty"
                            (i, (Node (key, value) left right )) -> if (treeSize left == i) then (key, value) else
                                                                    if (treeSize left > i) then (kMean i left) else (kMean (i - (treeSize left) - 1) right)


