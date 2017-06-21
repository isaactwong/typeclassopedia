data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

{-
13.2.1 There are at least two natural ways to turn a tree of lists into a list of trees. What are they, and why?
-}

-- example1 :: Tree ([a]) -> [Tree a]
-- example2 :: Tree ([a]) -> [Tree a]

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
