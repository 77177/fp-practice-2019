module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList revlist = case revlist of
                        RNil -> []
                        (RCons rlst element) -> concat [rlistToList rlst, [element]]

listToRList :: [a] -> ReverseList a
listToRList list  = case list of
                        [] -> RNil
                        list -> RCons (listToRList (init list)) (last list)

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
    show revlist = case revlist of
                        RNil -> ""
                        (RCons RNil element) -> show element
                        (RCons rlist element) -> show element ++ " " ++ show rlist

instance (Eq a) => Eq (ReverseList a) where

    (==) leftList rightList = (rlistToList leftList) == (rlistToList rightList)

instance (Ord a) => Ord (ReverseList a) where
    compare leftList rightList = compare (rlistToList leftList) (rlistToList rightList)

instance Monoid (ReverseList a) where

    mempty = RNil
    mappend leftList rightList = listToRList (mappend (rlistToList leftList) (rlistToList rightList))

instance Functor ReverseList where

    fmap f rlist = listToRList (fmap f (rlistToList rlist))
