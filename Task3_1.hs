module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

instance Show WeirdPeanoNumber where
    show Zero = "(zero)"
    show (Succ peanoNum) = concat (show peanoNum : "(succesoresor)" : [])
    show (Pred peanoNum) = concat ("(previous)": show peanoNum : [])

peanoToInteger :: WeirdPeanoNumber -> Integer
peanoToInteger peano = case peano of
                        Zero -> 0
                        (Succ peanoNum) -> (peanoToInteger peanoNum) + 1
                        (Pred peanoNum) -> (peanoToInteger peanoNum) - 1

peanoFromInteger :: Integer -> WeirdPeanoNumber
peanoFromInteger integer = if (integer == 0) then Zero else
                           if (integer > 0) then (Succ (peanoFromInteger (integer - 1))) else
                           if (integer < 0) then (Pred (peanoFromInteger (integer + 1))) else error "faulty integer"


instance Eq WeirdPeanoNumber where
    (==) peanoNum1 peanoNum2 = peanoToInteger peanoNum1 == peanoToInteger peanoNum2

instance Ord WeirdPeanoNumber where
    compare peanoNum1 peanoNum2 = if (peanoToInteger peanoNum1 == peanoToInteger peanoNum2) then EQ else
                                  if (peanoToInteger peanoNum1 > peanoToInteger peanoNum2) then (GT) else
                                  if (peanoToInteger peanoNum1 < peanoToInteger peanoNum2) then (LT) else error "faulty peano numbers"



instance Num WeirdPeanoNumber where
    (+) peanoNum1 peanoNum2 = case (peanoNum1, peanoNum2) of
                                    (peanoNumber,Zero) -> peanoNumber
                                    (peanoNumber,(Succ succesor)) -> Succ ((+) succesor peanoNumber)
                                    (peanoNumber,(Pred previous)) -> Pred ((+) previous peanoNumber)

    negate peanoNum  =  case (peanoNum) of
                            Zero -> Zero
                            (Succ succesor) -> Pred (negate succesor)
                            (Pred previous) -> Succ (negate previous)

    signum peanoNum = if ((peanoToInteger peanoNum) == 0) then (peanoFromInteger (0)) else
                      if ((peanoToInteger peanoNum) > 0) then (peanoFromInteger (1)) else (peanoFromInteger (-1))

    abs peanoNum = if ((peanoToInteger peanoNum) < 0)  then (peanoFromInteger (negate (peanoToInteger peanoNum))) else (peanoNum)

    fromInteger   = peanoFromInteger

    (*) peanoNum1 peanoNum2 = case (signum peanoNum2) of
                                Succ Zero -> peanoNum1 + (peanoNum1 * Pred peanoNum2)
                                Pred Zero -> negate (peanoNum1 * abs peanoNum2)
                                Zero -> Zero

instance Enum WeirdPeanoNumber where
  fromEnum = peanoFromEnum
  toEnum   = peanoToEnum

instance Real WeirdPeanoNumber where
  toRational = peanoToRational

instance Integral WeirdPeanoNumber where
    toInteger peano = case peano of
                        Zero -> 0
                        (Succ peanoNum) -> (toInteger peanoNum) + 1
                        (Pred peanoNum) -> (toInteger peanoNum) - 1

    quotRem peanoNum1 peanoNum2 =  if (peanoNum2 /= Zero)
                   then (wholePart, remainderPart)
                   else error ("division by zero is not permisisble")
                   where
                       wholePart = (peanoWhole (abs peanoNum1) (abs peanoNum2)) * ((signum peanoNum1) * (signum peanoNum2))
                       remainderPart = (peanoRemainder (abs peanoNum1) (abs peanoNum2)) * ((signum peanoNum1) * (signum peanoNum2))


peanoWhole peanoNum1 peanoNum2 = if ((peanoNum1 - peanoNum2) < Zero) then Zero else (quot (peanoNum1 - peanoNum2)  peanoNum2) + 1

peanoRemainder peanoNum1 peanoNum2 = if ((peanoNum1 - peanoNum2) < Zero) then peanoNum1 else peanoRemainder (peanoNum1 - peanoNum2) peanoNum2

peanoToRational :: WeirdPeanoNumber -> Rational
peanoToRational peanoNum = case peanoNum of
                                        Zero -> 0
                                        (Succ succesor) -> (peanoToRational succesor) + 1
                                        (Pred previous) -> (peanoToRational previous) - 1


peanoToEnum :: Int -> WeirdPeanoNumber
peanoToEnum peanoNum = if peanoNum == 0 then Zero else
                       if (peanoNum > 0) then Succ (peanoToEnum (peanoNum - 1)) else
                       (Pred (peanoToEnum (p + 1)))

peanoFromEnum :: WeirdPeanoNumber -> Int
peanoFromEnum peanoNum = case peanoNum of
                                Zero -> 0
                                (Succ succesor) -> (peanoFromEnum succesor) + 1
                                (Pred previous) -> (peanoFromEnum previous) - 1
