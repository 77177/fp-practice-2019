module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)


foldl :: (b -> a -> b) -> b -> [a] -> b

foldl function accumulator [] = accumulator
foldl function accumulator (firstElem:restOfList) = foldl function (function accumulator firstElem) restOfList

foldr :: (a -> b -> b) -> b -> [a] -> b

foldr function accumulator [] = accumulator
foldr function accumulator (firstElem:restOfList) = (function firstElem  (foldr function accumulator restOfList))


unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr function initial_value = case (function initial_value) of
              Just (value, changed_value) -> value : unfoldr function changed_value
              Nothing          -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map function list = foldr (\element restOfList -> function element : restOfList) [] list


-- Произведение всех элементов списка
product :: [Integer] -> Integer
product list = foldl (*) 1 list

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]

--for testing
maybeVal::Int -> Maybe Int
maybeVal 0 = Nothing
maybeVal x = Just x
maybeList = catMaybes [(maybeVal 1), (maybeVal 0), (maybeVal 3), (maybeVal 0)]
--

justFilter element restOfList = case element of
                                    Just val -> val : restOfList
                                    _ -> restOfList

catMaybes list = foldr justFilter [] list

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal list  = case list of
    (firstElem:restOfList) -> head firstElem : diagonal (map tail restOfList)
    [] -> []

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]

filterTrue predicate element restOfList = case predicate element of
 True -> element : restOfList
 False -> restOfList

filterNot predicate list = foldr (filterTrue predicate) [] list

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool

isTheElement elementToBeFound element restOfList = case element == elementToBeFound of
 True -> element : restOfList
 False -> restOfList

elem elementToFind list = foldr (||) False (map (\x -> if x == elementToFind then True else False) (foldr (isTheElement elementToFind) [] list))

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]

genList::Integer -> Integer ->[Integer] ->[Integer]

getListElement::[Integer] -> Integer ->Integer
getListElement list index = list !! (fromIntegral index)

genList step length list = map (getListElement list) (filterNot (\x -> if mod x step == 0 then True else False) $ init [0..length])

rangeTo from to step = genList step (toInteger (length (init [from..to]))) $ init [from..to]

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append leftList rightList = foldr (:) rightList leftList

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]

createSubList size [] = Nothing
createSubList size list = Just (take size list, drop size list)
groups lst n = (unfoldr (createSubList (fromIntegral n)) (lst))