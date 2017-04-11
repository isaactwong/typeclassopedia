import Control.Applicative

{-
4.2.1

One might imagine a variant of the interchange law that says something about applying a pure function to an effectful argument. Using the above laws, prove that
pure f <*> x = pure (flip ($)) <*> x <*> pure f
-}

-- TODO

class Functor f => MyApplicative f where
      pure' :: a -> f a
      --infix1 4 <@>, @>, <@
      (<@>) :: f (a -> b) -> f a -> f b

{-
      (@>) :: f a -> f b -> f b
      a1 @> a2 = (id <$ a1) <@> a2

      (<@) :: f a -> f b -> f a
      (<@) = liftA2 const
-}

-- 4.3.1 Define an applicative instance for Maybe
instance MyApplicative Maybe where
         pure' x = Just x
         Nothing <@> _ = Nothing
         _ <@> Nothing = Nothing
         Just f <@> Just x = Just (f x)

-- 4.3.2 Determine the correct definition of pure for the ZipList instance of Applicative—
-- there is only one implementation that satisfies the law relating pure and (<*>).
newtype ZippyList a = ZippyList { getZippyList :: [a] } deriving (Eq,Show)

instance Functor ZippyList where
         fmap f (ZippyList xs)= ZippyList (fmap f xs)

instance MyApplicative ZippyList where
         pure' x = ZippyList (repeat x)
         (ZippyList fs) <@> (ZippyList xs) = ZippyList (zipWith ($) fs xs)

{-
4.5.1 Implement a function sequenceAL :: Applicative f => [f a] -> f [a]

There is a generalized version of this, sequenceA, which works for any Traversable (see the later section on Traversable), but implementing this version specialized to lists is a good exercise.
-}

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL [] = pure []
sequenceAL (x:xs) = pure (:) <*> x <*> sequenceAL xs
