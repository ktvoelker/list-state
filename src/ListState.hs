
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module ListState where

import Control.Monad.State

newtype ListState s a = ListState { runListState :: s -> [(a, s)] }

evalListState :: ListState s a -> s -> [a]
evalListState m = map fst . runListState m

execListState :: ListState s a -> s -> [s]
execListState m = map snd . runListState m

instance Functor (ListState s) where
  fmap f m = ListState $ \s -> map (\(x, s) -> (f x, s)) $ runListState m s

instance Monad (ListState s) where
  (>>=) m f = ListState
    $ \s -> concatMap (\(x, s') -> runListState (f x) s') $ runListState m s
  return x = ListState $ \s -> [(x, s)]

instance MonadPlus (ListState s) where
  mzero = ListState $ \_ -> []
  mplus m1 m2 = ListState $ \s -> runListState m1 s ++ runListState m2 s

instance MonadState s (ListState s) where
  get = ListState $ \s -> [(s, s)]
  put s = ListState $ \_ -> [((), s)]

