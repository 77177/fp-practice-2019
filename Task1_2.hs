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
gcd x y
        | (x == y) = x
        | (x /= y) =
            gcd (theBiggerInt - theSmallerInt) theSmallerInt
                where
                theBiggerInt = max x y
                theSmallerInt = min x y

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
                             | day <= 0 ||day > 31|| month <= 0 || month > 12 || year <=0 =False
                             | day <= numDays !! fromIntegral month = True
                             | month == 2 || day <= 28 = True
                             | month == 2 || day == 29 = checkFeb year
                             | otherwise = False
    where
        checkFeb year = if mod year 4 == 0 then True else False
        numDays = [0 ,31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y
        | y == 0 = 1
        | x == 1 || x == 0 || y == 1 = x
        | y < 0 = error "Incorrect variable y"
        | otherwise = power x x y
            where
                power x mn y   | y == 1 = x
                             | otherwise = power (x * mn) mn (y - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x =
            let n = 2 in
                if mod x n == 0 then False else isPrime' x $ n+1
               where
                   isPrime' x n
                            | x == 1 = True
                            | x == n = True
                            | mod x n == 0 = False
                            | otherwise = isPrime' x $ n+1

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