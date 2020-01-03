module Task3_3 where

{-
  Задание 3.3
  Множество на основе предикатов
-}

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

--      3 закона моноида

-- применение mappend к пустому монойду не влияет на ответ
--     mempty `mappend` x = x
--     x `mappend` mempty = x

--  порядок выполнения mappend не дожен быть важен
--     (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- В соответствии с данными ограничениями реализую методы моноида.
-- За начальное множество - возьму множество с предикатом, который возварщает False.

instance Monoid (PSet a) where
    mempty = PSet (\arg -> False)
    mappend (PSet firstSetPredicate) (PSet secondSetPredicate) = PSet (\arg -> (firstSetPredicate arg) || (secondSetPredicate arg))

--      2 закона функтора
-- проход по функтору с помощью функции тождественного отображения возвращает неизменный функтор
--      fmap id = id

-- Если 2 последовательные операции обхода выподнятюся друг за другом,
-- то результат должен быть идентичен проходу 1-й композитной функции, которая состоит из этих двух
--      fmap (f . g)  ==  fmap f . fmap g

-- Стандратная реализация fmap состоит в
-- получении контейнера со значением -> распаковка контейнера и добыча значения -> примение к значению функции -> запаковака значения обратно в контейнер

instance Functor (PSet) where
    fmap func (PSet predicate) = PSet (func predicate)

-- Данный функтор не является рабочим ввиду того, что в декларации fmap  (fmap :: (a -> b) -> PSet a -> PSet b)
-- параметр 'a' свзяывается не связывается с типом предиката и выкидывет ошибку.
--
-- Couldn't match expected type ‘a’ with actual type ‘a -> Bool’
-- ‘a’ is a rigid type variable bound by
-- the type signature

