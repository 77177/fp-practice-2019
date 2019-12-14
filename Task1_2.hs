module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}
import Prelude hiding (pow, gcd)
import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y = if (x == y) then x else gcd (max (x) (y) - min (x) (y)) (min (x) (y))

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
getBool::Integer -> Double -> Bool

getBool a b = if ((sqrt(fromIntegral a)) - fromIntegral(floor(sqrt(fromIntegral a))) == 0) then True else False
isFullSquare:: Integer -> Bool
isFullSquare x = getBool x $ fromIntegral $ floor $ sqrt $ fromIntegral $ x
doesSquareBetweenExist from to = foldr (||) False $ map (\x -> isFullSquare x) $ init [from..to]

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y = if y /= 1 then x * pow (x) (y-1) else x

-- является ли данное число простым?
isPrime :: Integer -> Bool

isPrime x = case x of
        1 -> False
        2 -> True
        x -> if (length [y | y <- [2 .. x-1], mod x y == 0]) > 0 then False else True


type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
