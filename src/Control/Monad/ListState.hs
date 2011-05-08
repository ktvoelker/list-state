
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Control.Monad.ListState where

import Control.Monad.State

class (Monad m) => MonadList m where
  returns :: [a] -> m a

instance MonadList [] where
  returns = id

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

instance MonadList (ListState s) where
  returns xs = ListState $ zip xs . repeat

instance MonadPlus (ListState s) where
  mzero = ListState $ \_ -> []
  mplus m1 m2 = ListState $ \s -> runListState m1 s ++ runListState m2 s

instance MonadState s (ListState s) where
  get = ListState $ \s -> [(s, s)]
  put s = ListState $ \_ -> [((), s)]

newtype ListStateT s m a = ListStateT { runListStateT :: s -> m [(a, s)] }

evalListStateT :: (Monad m) => ListStateT s m a -> s -> m [a]
evalListStateT m = runListStateT m >=> return . map fst

execListStateT :: (Monad m) => ListStateT s m a -> s -> m [s]
execListStateT m = runListStateT m >=> return . map snd

instance (Functor m) => Functor (ListStateT s m) where
  fmap f m =
    ListStateT
    $ \s ->
      fmap (map (\(x, s) -> (f x, s)))
      $ runListStateT m s

instance (Monad m) => Monad (ListStateT s m) where
  (>>=) m f = ListStateT
    $ \s ->
      runListStateT m s
      >>= mapM (\(x, s') -> runListStateT (f x) s')
      >>= return . concat
  return x = ListStateT $ \s -> return [(x, s)]

instance (Monad m) => MonadList (ListStateT s m) where
  returns xs = ListStateT $ return . zip xs . repeat

instance (Monad m) => MonadPlus (ListStateT s m) where
  mzero = ListStateT $ \_ -> return []
  mplus m1 m2 =
    ListStateT
    $ \s -> do
      m1s <- runListStateT m1 s
      m2s <- runListStateT m2 s
      return $ m1s ++ m2s

instance (Monad m) => MonadState s (ListStateT s m) where
  get = ListStateT $ \s -> return [(s, s)]
  put s = ListStateT $ \_ -> return [((), s)]

instance MonadTrans (ListStateT s) where
  lift m = ListStateT $ \s -> m >>= \x -> return [(x, s)]

