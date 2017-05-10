{-
6.2.1 What is the kind of t in the declaration of MonadTrans?
-}

class MonadTrans t where
  lift :: Monad m => m a -> t m a

-- The kind of t is (*->*)->*->*
-- Because t takes a Monad which is of kind (*->*) and a value which is of type (*) and returns a value (*).