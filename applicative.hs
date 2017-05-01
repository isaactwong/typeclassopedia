import Control.Applicative

{-
4.2.1

One might imagine a variant of the interchange law that says something about applying a pure function to an effectful argument. Using the above laws, prove that
pure f <*> x = pure (flip ($)) <*> x <*> pure f

Hint from https://github.com/izubkov/Typeclassopedia/blob/master/Typeclassopedia.hs
-}

pure (flip ($)) <*> x <*> pure f                  =
(pure (flip ($)) <*> x) <*> pure f                =
pure ($ f) <*> (pure (flip ($)) <*> x)            =
pure (.) <*> pure ($ f) <*> pure (flip ($)) <*> x = 
pure ((.) ($ f)) <*> pure (flip ($)) <*> x        =
pure ((.) ($ f) (flip ($))) <*> x                 =
pure (($ f) . flip ($)) <*> x                     =
pure f <*> x

-- all this imples that
pure (flip ($)) <*> x <*> pure f = pure f <*> x -- QED

class Functor f => MyApplicative f where
      pure' :: a -> f a
      (<@>) :: f (a -> b) -> f a -> f b

{-
4.3.1 Define an applicative instance for Maybe
-}

instance MyApplicative Maybe where
         pure' x           = Just x
         Nothing <@> _     = Nothing
         _ <@> Nothing     = Nothing
         Just f <@> Just x = Just (f x)

{-
4.3.2 Determine the correct definition of pure for the ZipList instance of Applicative—
there is only one implementation that satisfies the law relating pure and (<*>).
-}

newtype ZippyList a = ZippyList { getZippyList :: [a] } deriving (Eq,Show)

instance Functor ZippyList where
         fmap f (ZippyList xs) = ZippyList (fmap f xs)

instance MyApplicative ZippyList where
         pure' x = ZippyList (repeat x)
         (ZippyList fs) <@> (ZippyList xs) = ZippyList (zipWith ($) fs xs)

{-
4.5.1 Implement a function sequenceAL :: Applicative f => [f a] -> f [a]

There is a generalized version of this, sequenceA, which works for any Traversable (see the later section on Traversable), but implementing this version specialized to lists is a good exercise.
-}

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL []     = pure []
sequenceAL (x:xs) = pure (:) <*> x <*> sequenceAL xs

{-
4.6.1 Implement pure and (<*>) in terms of unit and (**), and vice versa.
-}

class Functor f => Monoidal f where
      unit :: f ()
      (**) :: f a -> f b -> f (a,b)

-- Implementations of pure and <*> in terms of unit and **
pure a    = fmap (const a) unit
ff <*> fa = fmap (uncurry $) (ff ** fa)

-- Implentations of unit and ** in terms of pure and <*>
unit = pure ()
fa ** fb = pure (,) <*> fa <*> fb

{-
4.6.2
Are there any Applicative instances for which there are also functions
f () -> () and f (a,b) -> (f a, f b), satisfying some "reasonable" laws?
-}

instance Monoidal Maybe where
         unit                 = Just ()
         Nothing ** _         = Nothing
         _ ** Nothing         = Nothing
         (Just x) ** (Just y) = Just (x,y)

unit' :: Maybe () -> ()
unit' Nothing   = ()
unit' (Just ()) = ()

unjoin :: Maybe (a,b) -> (Maybe a, Maybe b)
unjoin Nothing      = (Nothing, Nothing)
unjoin (Just (x,y)) = (Just x, Just y)

{- 
Identity Laws
unit . unit' = ()
fst . unjoin (f a ** f b) = f a
snd . unjoin (f a ** f b) = f b
-}

-- 4.6.3 (Tricky) Prove that given your implementations from the first exercise, the usual Applicative laws and the Monoidal laws stated above are equivalent.

-- Proving the Monodial Laws first
unit ** v = pure (,) <*> unit <*> v
          = pure (,) <*> pure () <*> v
          = pure ((),) <*> v
          = pure ((),v) `isomorphic` v
v ** unit = pure (,) <*> v <*> unit
          = pure (,) <*> v <*> pure ()
          = pure (v,) <*> pure ()
          = pure (v,()) `isomorphic` v
u ** (v ** w) = pure (,) <*> u <*> (pure (,) <*> v <*> w)
              = pure (,) <*> pure $ (,) <*> u <*> v <*> w
              = pure (,) <*> (u ** v) <*> w
              = (u ** v) <*> w

