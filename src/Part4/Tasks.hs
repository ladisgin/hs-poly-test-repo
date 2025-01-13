{-# LANGUAGE InstanceSigs #-}
module Part4.Tasks where

import Util(notImplementedYet, NotImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst = reverseToRlist (reverse lst)
    where
        reverseToRlist [] = REmpty
        reverseToRlist (f : t) = reverseToRlist t :< f

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show rlist = "[" ++ showHelper rlist ++ "]"
        where
            showHelper REmpty = ""
            showHelper (REmpty :< t) = show t
            showHelper (h :< t) = showHelper h ++ "," ++ show t

instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) (h :< t) REmpty = False
    (==) REmpty (h :< t) = False
    (==) (lh :< lt) (rh :< rt) = lt == rt && lh == rh

instance Semigroup (ReverseList a) where
    (<>) a REmpty = a
    (<>) a (b :< t) = (a <> b) :< t

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap f REmpty = REmpty
    fmap f (fr :< t) = fmap f fr :< f t

instance Applicative ReverseList where
    pure t  = REmpty :< t
    (<*>) REmpty _ = REmpty
    (<*>) _ REmpty = REmpty
    (<*>) (f :< t) lst = (f <*> lst) <> (t <$> lst)

instance Monad ReverseList where
    (>>=) REmpty t = REmpty
    (>>=) (fr :< t) f = (fr >>= f) <> f t
