
{-
4.2.1

One might imagine a variant of the interchange law that says something about applying a pure function to an effectful argument. Using the above laws, prove that
pure f <*> x = pure (flip ($)) <*> x <*> pure f
-}

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
newtype ZipList a = ZipList { getZipList :: [a] } deriving (Eq,Show)

instance Functor ZipList where
         fmap f (ZipList xs)= ZipList (fmap f xs)

instance MyApplicative ZipList where
         pure' x = ZipList (repeat x)
         (ZipList fs) <@> (ZipList xs) = ZipList (zipWith ($) fs xs)