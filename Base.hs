{-# LANGUAGE DeriveFunctor, FunctionalDependencies, FlexibleContexts #-}
module Base where
import qualified Streaming as S

data F a = F { unF :: Int -> (Int, a) }
  deriving Functor

instance Monad F where
  return a = F (\n -> (n,a))
  F sa >>= f = F $ \s -> let (s', a) = sa s in unF (f a) s'

instance Applicative F where pure = undefined ; (<*>) = undefined
get :: MonadFree F free => free Int
get = wrap . F $ \s -> (s, return s)

put :: MonadFree F free => Int -> free ()
put s = wrap . F $ \_ -> (s, return ())

class Monad m => MonadFree f m | m -> f where
  wrap :: f (m a) -> m a

instance (Functor f, Monad m) => MonadFree f (S.Stream f m) where
  wrap = S.wrap

