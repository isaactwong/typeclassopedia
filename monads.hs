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

{-
5.3.1
Implement (>>=) in terms of fmap (or liftM) and join.
-}

(>>=) :: (Applicative m) => m a -> (a -> m b) -> m b
m >>= f = join (fmap f m)


{-
5.3.2
Now implement join and fmap (liftM) in terms of (>>=) and return
-}

join :: (Monad m) => m (m a) -> m a
join m = m >>= id


fmap :: (Monad m) => (a -> b) -> m a -> m b
fmap f m = m >>= (return . f)

{-
5.5.1
Given the definition g >=> h = \x -> g x >>= h, prove the equivalence of the above laws and the usual monad laws.
-}

-- Assuming the >=> laws we derive using the definition of >=>:
return >==> g = g            -- implies that, using the definition of >=>
(\x -> return x) >>= g = \x -> g x -- which implies
return x >>= g = g           -- which proves the first monad law. Note that since implications go 
                             -- in the other direction as well that the monad laws imply the laws for >=>

-- For the second monad law we use a similar derivation, beginning with the second >=> law
g >=> return = g                   -- implies that, using the definition of >=>
(\x -> g x) >>= return = \x -> g x -- and letting g x = m, this implies that
m >>= return = m                   -- which proves the second monad law. Again, implications go in the other
                                   -- other direction as well, showing that the monad laws imply the >=> laws.

-- We now prove tghe associativity of the third law.






