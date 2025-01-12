module Part1.Tasks where

import Util(notImplementedYet)
import Data.Fixed (mod')

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

normalizeAngle :: Double -> Double
normalizeAngle x = x - 2 * pi * fromIntegral (floor (x / (2 * pi)))

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sum $ takeWhile (\a -> abs a > 1e-10) [((-1) ^ n * normalizeAngle x ^ (2 * n + 1)) / fromIntegral (factorial (2 * n + 1)) | n <- [0..]]

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = sum $ takeWhile (\a -> abs a > 1e-10) [((-1) ^ n * normalizeAngle x ^ (2 * n)) / fromIntegral (factorial (2 * n)) | n <- [0..]]

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b (mod a b)

daysInFebruary :: Integer -> Integer
daysInFebruary y
  | mod y 400 == 0 = 29
  | mod y 100 == 0 = 28
  | mod y 4 == 0 = 29
  | otherwise = 28

daysInMonth :: Integer -> Integer -> Integer
daysInMonth y m
  | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | m == 2 = daysInFebruary y
  | otherwise = 30

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y
  |  d <= daysInMonth y m = True
  | otherwise = False

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow a b
  | even b = myPow (a * a) (div b 2)
  | otherwise = a * myPow a (b - 1)

divides :: Integer -> Integer -> Bool
divides x y = y `mod` x == 0

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime a
  | a < 1 = False
  | otherwise = not (any (`divides` a) [2..(a - 1)])

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = abs (sum terms) / 2
  where
    closedPoints = points ++ [head points]
    terms = [x1 * y2 - y1 * x2 | ((x1, y1), (x2, y2)) <- zip closedPoints (tail closedPoints)]


-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | a < b = triangleKind b a c
  | b < c = triangleKind a c b
  | a < c = triangleKind c b a
  | a > b + c = -1
  | a * a > b * b + c * c = 0
  | a * a < b * b + c * c = 1
  | otherwise = 2