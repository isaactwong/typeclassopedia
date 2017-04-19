{-
5.2.1
Implement a Monad instance for the list constructor, []. Follow the types!

instance Monad [] where
         return' x = [x]
         m >>= f  = concat (map f m)
-}

{-
5.2.2
Implement a Monad instance for ((->) e).
instance Monad ((->) e) where
         return  = const
         m >>= f = \e -> f (m e) e
-}