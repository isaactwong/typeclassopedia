-- 1. Implement the Functor instances for Either e and ((->) e)

instance Functor (Either e) where
         fmap f (Right b) = Right (f b)
         fmap _ (Left a)  = a

instance Functor ((->) e) where
         fmap g f = f . g
--       fmap = (.)

-- 2. Implement the Functor instances for ((,) e) and for Pair, defined as
-- data Pair a = Pair a a
-- Explain their similarities and differences.

instance Functor ((,) e) where
         fmap f (e,a) = (e,(f a))
         
data Pair a = Pair a a
instance Functor Pair where 
         fmap f (Pair a a) = Pair (f a) (f a)

{-
Both are Functors of an ordered pair, but the first is only "functorial" in the second variable while the second is "functorial" in both variables. This is mostly because the first can be built out of essentially 2 types, while the second is only built out of one type.
-}