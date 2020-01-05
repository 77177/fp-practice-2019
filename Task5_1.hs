module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec




-- Реализуйте функции индексирования, вставки и удаления элементов

dlist2list :: DList a -> [a]
dlist2list DNil = []
dlist2list (DCons left h right) = concat [[h], dlist2list right]

test = DCons (DNil) (1) (DNil)

index :: DList a -> Int -> a
index _ i | i < 0 = error "index is less than zero"
index (DCons left h right) ind  | ind /= 0 = index (right) (ind - 1)
                                | ind == 0 = h

insertAt :: DList a -> Int -> a -> DList a
insertAt _ i _ | i < 0 = error "index is less than zero"
insertAt (DCons left h right) index value | index /= 0 = (DCons left h (insertAt right (index - 1) value))
                                          | index == 0 = DCons left value (list2dlist (h : dlist2list right))

getList lst = (tail (dlist2list lst))
removeAt :: DList a -> Int -> DList a
removeAt _ i | i < 0 = error "index is less than zero"

removeAt (DCons left h (DCons left1 v DNil)) index | index /= 1 = error "error out of bounds"
                                                   | index == 1 = (DCons left h DNil)


removeAt (DCons left h right) index | index /= 0 = (DCons left h (removeAt right (index-1)))
                                    | index == 0 = (DCons left firstRight otherRight)
                                    where
                                      firstRight = head (dlist2list right)
                                      otherRight = list2dlist (tail (dlist2list right))
