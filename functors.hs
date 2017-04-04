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

{-
3. Implement a Functor instance for the type ITree, defined as
data ITree a = Leaf (Int -> a) 
             | Node [ITree a]
-}
data ITree a = Leaf (Int -> a) | Node [ITree a]
instance Functor ITree where
         fmap f (Leaf g)  = Lead (f . g)
         fmap f (Node ns) = Node (map (fmap f) ns)
         
{-
4. Give an example of a type of kind * -> * which cannot be made an instance of Functor (without using undefined).
Help from http://stackoverflow.com/questions/7220436/good-examples-of-not-a-functor-functor-applicative-monad
-}
data T a = T (a -> Int)

-- Can't write fmap. Given f :: a -> b, can't get T a -> T b

{-
5. Is this statement true or false? The composition of two Functors is also a Functor.
If false, give a counterexample; if true, prove it by exhibiting some appropriate Haskell code. 
-}
-- True!