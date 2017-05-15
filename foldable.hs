{-
11.2.1 What is the type of foldMap . foldMap? Or foldMap . foldMap . foldMap, etc.? What do they do?
-}

(foldMap . foldMap) :: (Foldable t, Foldable t2) => (a -> m) -> t (t2 a) -> b
(foldMap . foldMap . foldMap) :: (Foldable t, Foldable t2, Foldable t3) => (a -> m) -> t (t2 (t3 a)) -> b

-- Continually reduces nested structures to a single value.

{-
11.3.1 Implement toList :: Foldable f => f a -> [a].
-}

toList :: (Foldable t) => f a -> [a]
toList = foldmap (:[])