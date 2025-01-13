{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map
import Data.Maybe (fromMaybe)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       from2list :: [[Int]] -> mx
       getElement :: mx -> (Int, Int) -> Int
       getDimension :: mx -> (Int, Int)

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       from2list [[e]] = e
       getElement el _ = el
       getDimension _ = (1, 1)

instance Matrix [[Int]] where
       from2list a = a
       getElement m (x, y) = (m !! x) !! y
       getDimension [] = (0, 0)
       getDimension m@(h : t) = (length h, length m)

instance Matrix (SparseMatrix Int) where
       from2list a = SparseMatrix {
               sparseMatrixWidth = length a,
               sparseMatrixHeight = length (head a),
               sparseMatrixElements = Data.Map.fromList [((r, c), val) | (r, row) <- zip [0..] a, (c, val) <- zip [0..] row, val /= 0]
       }
       getElement (SparseMatrix _ _ elements) (x, y) = fromMaybe 0 (Data.Map.lookup (x, y) elements)
       getDimension m = (sparseMatrixWidth m, sparseMatrixHeight m)

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = from2list [[if i == j then 1 else 0 | j <- [1..w]] | i <- [1..w]]

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = from2list [[0 | j <- [1..w]] | i <- [1..h]]

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b = from2list [[ sum [ getElement a (ai, aj) * getElement b (aj, bj) | aj <- [0..(fst (getDimension a) - 1)]] | bj <- [0..(fst (getDimension b) - 1)]] | ai <- [0..(snd (getDimension a) - 1)]]


minor :: Matrix m => m -> Int -> Int -> m
minor m row col =
  let (w, h) = getDimension m
  in from2list [[getElement m (i, j) | j <- [0..h-1], j /= col] | i <- [0..w-1], i /= row]

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m =
       let (w, h) = getDimension m
       in case h of
              1 -> getElement m (0, 0)
              2 -> let a = getElement m (0, 0)
                       b = getElement m (0, 1)
                       c = getElement m (1, 0)
                       d = getElement m (1, 1)
                   in a * d - b * c
              _ -> sum [(-1) ^ j * getElement m (0, j) * determinant (minor m 0 j) | j <- [0..w-1]]

