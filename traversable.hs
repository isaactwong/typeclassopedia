data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Show)

t1 = Node (Leaf [2,3]) [1] (Node (Leaf [6]) [4,5] (Leaf [7,8]))
t2 = Node (Leaf (Just 2)) (Just 4) (Node (Leaf (Just 5)) (Just 7) (Leaf (Just 8)))
t3 = Node (Leaf (Just 2)) (Just 4) (Node (Leaf (Just 5)) (Just 7) (Leaf (Nothing)))

instance Functor Tree where
  fmap f Empty               = Empty
  fmap f (Leaf a)            = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  foldMap f Empty               = mempty
  foldMap f (Leaf a)            = f a
  foldMap f (Node left a right) = (foldMap f left) `mappend` (f a) `mappend` (foldMap f right)

instance Traversable Tree where
  sequenceA Empty               = pure Empty
  sequenceA (Leaf a)            = Leaf <$> a
  sequenceA (Node left a right) = Node <$> (sequenceA left) <*> a <*> (sequenceA right)

{-
13.2.1 There are at least two natural ways to turn a tree of lists into a list of trees. What are they, and why?
-}

example1 :: Tree ([a]) -> [Tree a]
example1 = sequenceA

example2 :: Tree ([a]) -> [Tree a]
example2 = foldMap (fmap Leaf)

{-
13.2.2 Give a natural way to turn a list of trees into a tree of lists.
-}
toList :: Tree a -> [a]
toList Empty = []
toList (Leaf a)  = [a]
toList (Node left a right) = (toList left) ++ [a] ++ (toList right)

example3 :: [Tree a] -> Tree [a]
example3 ts = Leaf (foldr (\t acc -> (toList t) ++ acc) [] ts)
  
{-
13.2.3 What is the type of traverse . traverse? What does it do?
-}
{- 
(traverse . traverse) :: (Traversable t1, Traversable t2, Applicative f) => (a -> f b) -> t1 (t2 a) -> f (t1 (t2 b))

Because (.) :: (b -> c) -> (a -> b) -> (a -> c)
and traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
and arrow are right associative
(t a -> f (t b)) -> (t (t a) -> f (t (t b))) -> 
(a -> f b) -> (t a -> f (t b))               ->
(a -> f b) -> t (t a) -> f (t (t b))

This is traverse-ing a nested set of two traversable structures.
-}

{-
13.2.4 Implement traverse in terms of sequenceA, and vice versa.
-}

traverse_ :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse_ f = sequenceA . fmap f

sequenceA_ :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA_ = traverse_ id

{-
13.3.1 Implement fmap and foldMap using only the Traversable methods. (Note that the Traversable module provides these implementations as fmapDefault and foldMapDefault.)
-}
-- Todo

{-
13.3.2 Implement Traversable instances for [], Maybe, ((,) e), and Either e.
-}
traverseArray :: (Applicative f) => (a -> f b) -> [a] -> f [b]
traverseArray f = foldr (\x xs -> (:) <$> (f(x)) <*> xs) (pure [])

traverseMaybe :: (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
traverseMaybe _ Nothing = pure Nothing
traverseMaybe f (Just x) = Just <$> (f x)

traverseTuple :: (Applicative f) => (a -> f b) -> (e, a) -> f (e, b)
traverseTuple f (e, x) = ((,) e) <$> f x

traverseEither :: (Applicative f) => (a -> f b) -> Either e a -> f (Either e b)
traverseEither _ (Left e) = pure (Left e)
traverseEither f (Right x) = Right <$> f x

{-
13.3.3 Explain why Set is Foldable but not Traversable.

Foldable's don't have to be Functors. But to be Traversable you need to be a Functor as well.
Set has a constraint of Ord on the map function, and since there are no such constraints on Functor, then Set can't be a Functor. So Set can't be Traversable.

-}

{-
13.3.4 Show that Traversable functors compose: that is, implement an instance for Traversable (Compose f g) given Traversable instances for f and g.
-}

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose ((fmap . fmap) f x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose x) = (foldMap . foldMap) f x

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose t) = Compose <$> (traverse . traverse) f t

