{-
5.2.1
Implement a Monad instance for the list constructor, []. Follow the types!
-}

instance Monad [] where
         return' x = [x]
         m >>= f  = concat (map f m)

{-
5.2.2
Implement a Monad instance for ((->) e).
-}
instance Monad ((->) e) where
         return  = const
         m >>= f = \e -> f (m e) e

{-
5.2.3
Implement Functor and Monad instances for Free f, defined as
data Free f a = Var a
              | Node (f (Free f a))
You may assume that f has a Functor instance. This is known as the free monad built from the functor f.
-}

data Free f a = Var a | Node (f (Free f a))

instance (Functor f) => Functor (Free f) where
         fmap g (Var x)  = Var (g x)
         fmap g (Node x) = Node (fmap (fmap g) x)

instance (Functor f) => Monad (Free f) where
         return x       = (Var x)
         (Node m) >>= g = Node (fmap (>>=g) m)