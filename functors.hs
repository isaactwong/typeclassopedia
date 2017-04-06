import Data.Functor.Compose

class MyFunctor f where
      fmap' :: (a -> b) -> f a -> f b


-- 1. Implement the Functor instances for Either e and ((->) e)
instance MyFunctor (Either e) where
         fmap' f (Right b) = Right (f b)
         fmap' _ (Left a)  = Left a

instance MyFunctor ((->) e) where
         fmap' g f = g . f
--       fmap' = (.)



-- 2. Implement the Functor instances for ((,) e) and for Pair, defined as
-- data Pair a = Pair a a
-- Explain their similarities and differences.
instance MyFunctor ((,) e) where
         fmap' f (e,a) = (e,(f a))
         
data     Pair a = Pair a a
instance MyFunctor Pair where 
         fmap' f (Pair a b) = Pair (f a) (f b)

{-
Both are Functors of an ordered pair, but the first is only "functorial" in the second variable while the second is "functorial" in both variables. This is mostly because the first can be built out of essentially 2 types, while the second is only built out of one type.
-}

{-
3. Implement a Functor instance for the type ITree, defined as
data ITree a = Leaf (Int -> a) | Node [ITree a]
-}
data     ITree a = Leaf (Int -> a) | Node [ITree a]
instance MyFunctor ITree where
         fmap' f (Leaf g)  = Leaf (f . g)
         fmap' f (Node ns) = Node (map (fmap' f) ns)
         
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

{- 
True!
F and G are functors.
So, still working on sytax here, but given 2 functors, F, G, and a function f: a -> b, then I want to get a lifted function from F (G a) -> F (G b), which seems to me to be two fmap's.

instance (Functor f, Functor g) => Functor (f (g)) where
         fmap h x = fmap (fmap h) x

-- And from the standard prelude a very beautiful example that took me a long time to understand.
newtype  Compose f g a = Compose { getCompose :: f (g a)}
instance (Functor f, Functor g) => Functor (Compose f g) where
         fmap h (Compose x) = Compose (fmap (fmap h) x)
-}

{-
Functor Laws
1. Although it is not possible for a Functor instance to satisfy the first Functor law but not the second (excluding undefined), the reverse is possible. Give an example of a (bogus) Functor instance which satisfies the second law but not the first.
-}

-- Clever example from typeclassopedia
data     Break a = Yes | No deriving (Eq)
instance MyFunctor Break where
         fmap' f _ = No

-- fmap id Yes == Yes -- False
-- fmap (id . (const Yes)) Yes == (fmap id . fmap (const Yes)) Yes -- True

{-
Functor Laws
2. Which laws are violated by the evil Functor instance for list shown above: both laws, or the first law alone? Give specific counterexamples.
-}
-- Breaks f first law, but not the second.
instance MyFunctor [] where
         fmap' f []     = []
         fmap' f (x:xs) = (f x) : (f x) : (fmap' f xs)

-- fmap id [1,2,3] = [1,1,2,2,3,3] /= [1,2,3]
-- fmap ((+1).(*2)) [1,2,3] = [3,3,5,5,7,7] /= [3,3,3,3,5,5,5,5,7,7,7,7] = (fmap (+1) . fmap (*2)) [1,2,3]