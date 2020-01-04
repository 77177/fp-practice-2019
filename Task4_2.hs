module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf {one::a,two::a,three::a,four::a} deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12



-- функтор имеет стандартное поведение - просто берем все значения контейнера и применяем к ним функцию
-- функтор должен попадать под ограничения:

-- проход по функтору с помощью функции тождественного отображения возвращает неизменный функтор
--      fmap id = id

-- Если 2 последовательные операции обхода выподнятюся друг за другом,
-- то результат должен быть идентичен проходу 1-й композитной функции, которая состоит из этих двух
--      fmap (f . g)  ==  fmap f . fmap g


instance Functor FourOf where
    fmap func (FourOf first second third fourth) = (FourOf (func first) (func second) (func third) (func fourth))

-- аппликативный функтор имеет схожее поведение. На этот раз функции, которые сами находятся в контейнерах применяются по одной
-- для каждого аргумента из функтора

-- аппликативный функтор так же попадает под ограничения

-- функция pure просто берет элемен и заполняет им все места в FourOf


-- pure id <*> v = v
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)


instance Applicative FourOf where
    pure a = FourOf a a a a
    (<*>) (FourOf frst sec thrd frth) (FourOf a b c d) = (FourOf (frst a) (sec b) (thrd c) (frth d))


-- Для имплементации функциональности монады нужно реализовать функцию bind и return.
-- bind будет брать экземпляр FourOf и для каждого и 4-х значений делать монаду, из которой потом нужно взять соответствующее позиции
-- значения. После этого все 4 значения снова обварачиваются в монаду.

-- (>>=) :: m a -> (a -> m b) -> m b

-- при данной имплементации все ограничения монад ((return x >>= f) = f x, ассоциативность bind функции и (x >>= (\a -> return a)) = x) соблюдены

instance Monad FourOf where
    return = pure
    (>>=) (FourOf frst sec thrd frth) func = FourOf (one (func frst)) (two (func sec)) (three (func thrd)) (four (func frth))

-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf {one = 5, two = 8, three = 10, four = 12}
