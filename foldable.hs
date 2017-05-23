{-
11.2.1 What is the type of foldMap . foldMap? Or foldMap . foldMap . foldMap, etc.? What do they do?
-}

twoFold :: (Monoid m, Foldable t, Foldable t2) => (a -> m) -> t (t2 a) -> m
twoFold = (foldMap . foldMap)

threeFold :: (Monoid m, Foldable t, Foldable t2, Foldable t3) => (a -> m) -> t (t2 (t3 a)) -> m
threeFold = (foldMap . foldMap . foldMap)

-- Continually reduces nested structures to a single value.

{-
11.3.1 Implement toList :: Foldable f => f a -> [a].
-}

toList :: (Foldable f) => f a -> [a]
toList = foldMap (:[])

{-
11.3.2 Pick some of the following functions to implement: concat, concatMap, and, or, any, all, sum, product, maximum(By), minimum(By), elem, notElem, and find. Figure out how they generalize to Foldable and come up with elegant implementations using fold or foldMap along with appropriate Monoid instances.
-}

concat :: (Foldable t) => t [a] -> [a]
concat xs = foldMap id xs

concatMap :: (Foldable t) => (a -> [b]) -> t a -> [b]
concatMap f xs = foldMap f xs

newtype All = All { getAll :: Bool } deriving (Eq, Show, Ord, Read)
instance Monoid All where
         mempty                  = All True
         mappend (All x) (All y) = All (x && y)

and :: (Foldable t) => t Bool -> Bool
and = getAll . foldMap All 

newtype Any = Any { getAny :: Bool } deriving (Eq, Show, Ord, Read)
instance Monoid Any where
         mempty               = Any False
         mappend (Any True) _ = Any True
         mappend _ (Any True) = Any True
         mappend _ _          = Any False

any :: (Foldable t) => t Bool -> Bool
any = getAny . foldMap Any
         