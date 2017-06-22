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
-- todo
  
{-
13.2.3 What is the type of traverse . traverse? What does it do?
-}
-- todo

{-
13.2.4 Implement traverse in terms of sequenceA, and vice versa.
-}

traverse_ :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse_ f = sequenceA . fmap f

sequenceA_ :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA_ = traverse_ id
