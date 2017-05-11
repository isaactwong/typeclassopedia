{-
6.2.1 What is the kind of t in the declaration of MonadTrans?
-}

class MonadTrans t where
  lift :: Monad m => m a -> t m a

-- The kind of t is (*->*)->*->*
-- Because t takes a Monad which is of kind (*->*) and a value which is of type (*) and returns a value (*).

{-
6.4.1 Implement join :: M (N (M (N a))) -> M (N a), given distrib :: N (M a) -> M (N a) and assuming M and N are instances of Monad.
-}

join :: M (N (M (N a))) -> M (N a)
join m = join ((((fmap . fmap) join) (fmap distrib m)))
-- TODO Holy cow, is this right? How can we simplify it?