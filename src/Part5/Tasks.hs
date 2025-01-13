module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f b [] = b
myFoldl f b (fr : t) = myFoldl f (f b fr) t

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b [] = b
myFoldr f b (fr : t) = f fr (myFoldr f b t)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldl (\fr nx -> fr ++ [f nx]) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldl (\fr nx -> fr ++ f nx) []

myConcat :: [[a]] -> [a]
myConcat = myFoldl (++) []

myReverse :: [a] -> [a]
myReverse = myFoldl (\fr nx -> nx : fr) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldl (\fr nx -> if p nx then fr ++ [nx] else fr) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldl (\(fr, fr2) nx -> if p nx then (fr ++ [nx], fr2) else (fr, fr2 ++ [nx])) ([], [])
