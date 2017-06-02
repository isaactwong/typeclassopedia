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

newtype Any = Any { getAny :: Bool } deriving (Eq, Show, Ord, Read)
instance Monoid Any where
         mempty               = Any False
         mappend (Any True) _ = Any True
         mappend _ (Any True) = Any True
         mappend _ _          = Any False

and :: (Foldable t) => t Bool -> Bool
and = getAll . foldMap All 

or :: (Foldable t) => t Bool -> Bool
or = getAny . foldMap Any 

any :: (Foldable t) => (a -> Bool) -> t a -> Bool
any f = getAny . foldMap (Any . f) 

all :: (Foldable t) => (a -> Bool) -> t a -> Bool
all f = getAll . foldMap (All . f) 

newtype Sum = Sum { getSum :: Int } deriving (Eq, Show, Ord, Read)
instance Monoid Sum where
         mempty = Sum 0
         mappend (Sum x) (Sum y) = Sum (x + y)

newtype Product = Product { getProd :: Int } deriving (Eq, Show, Ord, Read)
instance Monoid Product where
         mempty = Product 1 
         mappend (Product x) (Product y) = Product (x * y)

sum :: (Foldable t) => t Int -> Int
sum = getSum . foldMap Sum

product :: (Foldable t) => t Int -> Int
product = getProd . foldMap Product

newtype Max = Max { getMax :: Int } deriving (Eq, Show, Ord, Read)
instance Monoid Max where
         mempty = Max (minBound :: Int)
         mappend (Max x) (Max y) | x < y     = Max y
                                 | otherwise = Max x

newtype Min = Min { getMin :: Int } deriving (Eq, Show, Ord, Read)
instance Monoid Min where
         mempty = Min (maxBound :: Int)
         mappend (Min x) (Min y) | x < y     = Min x
                                 | otherwise = Min y
         

maximum :: (Foldable t) => t Int -> Int
maximum = getMax . foldMap Max

minimum :: (Foldable t) => t Int -> Int
minimum = getMin . foldMap Min

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = Main.any (==x) 

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x = Main.all (/=x) 

newtype First a = First { getFirst :: Maybe a } deriving (Eq, Ord, Read, Show)
instance Monoid (First a) where
  mempty = First Nothing
  mappend (First Nothing) r = r
  mappend l _ = l

find :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
find f = getFirst . foldMap (\x -> First (if (f x) then (Just x) else Nothing))




  
{-
remember to do these 11.2.1 - 11.24
i missed these somehow

Implement fold in terms of foldMap.
What would you need in order to implement foldMap in terms of fold?
Implement foldMap in terms of foldr.
Implement foldr in terms of foldMap (hint: use the Endo monoid).
-}
