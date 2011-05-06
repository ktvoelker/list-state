
{--
 - WARNING: this doesn't compile! And there's probably no way to make it
 - compile without ruining the semantics I was trying to achieve.
 -
 - See ListState.hs for a more limited (but working) implementation.
 -}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module ListT where

import Control.Monad.State
import Control.Monad.Trans

{-- For reference:
instance Monad [] where
  m >>= f = concatMap f m
  return x = [x]
 --}

newtype ListT m a = ListT { runListT :: [m a] }

instance (Monad m) => Monad (ListT m) where
  (>>=) (ListT xs) f = map (\m -> h m $ g m) xs
    where
      g m = m >>= return . runListT . f
      h m g = map (i m g) [0 ..]
      i m g n = do
        mbs <- g
        let cur = listToMaybe $ drop n mbs
        return $ cur >>= (m >>=)
  return x = ListT [return x]

instance MonadTrans ListT where
  lift m = ListT [m]

instance MonadState s (ListT (State s)) where
  get = lift get
  put = lift . put

