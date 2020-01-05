module Task5_2 where

import Todo(todo)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа
instance (Show a) => Show (Zipper a) where
  show (Zipper left right) = "Zipper " ++ (show left) ++ " " ++ (show right)

test1 = Zipper [2,1] [3,4]
test2 = Zipper [6,5] [7,8]

helper :: Zipper a -> [a]
helper (Zipper [] full) = full

toList :: Zipper a -> [a]
toList zipper = helper (goLeftToTheEnd zipper)

instance (Eq a) => Eq (Zipper a) where
  (==) zipper1 zipper2 = (toList zipper1) == (toList zipper2)



fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

concatZipper :: Zipper a -> Zipper a -> Zipper a
concatZipper (Zipper (l1) (r1)) (Zipper (l2) (r2)) = Zipper (l1 ++ r1) (l2 ++ r2)

goLeftToTheEnd :: Zipper a -> Zipper a
goLeftToTheEnd into = case into of
                                  Zipper [] _ -> into
                                  Zipper left right -> goLeftToTheEnd (goLeft into)

insertFromBeginning :: Int -> Zipper a -> Zipper a -> Zipper a
insertFromBeginning index what into | index /= 0 = insertFromBeginning (index - 1) what (goRight into)
                                    | otherwise = case (into, what) of
                                                    ((Zipper left right),(Zipper empty full)) ->  Zipper (left) (full ++ right)

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index what into = insertFromBeginning index (goLeftToTheEnd what) (goLeftToTheEnd into)

getSubZipper from to (Zipper left right) = Zipper [] (take (to - from) (drop from (right)))

subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to input = getSubZipper from to (goLeftToTheEnd input)
