{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE UndecidableInstances   #-}

{- |
Module       :  Control.Monad.Random.Class
Copyright    :  (c) Cale Gibbard 2006-2007,
                (c) Russell O'Connor, Dan Doel and Remi Turk 2006,
                (c) Eric Kidd 2007
License      :  MIT-style (see the file LICENSE)

Maintainer   :  byorgey@gmail.com
Stability    :  experimental
Portability  :  non-portable (multi-param classes, functional dependencies, undecidable instances)

MonadRandom and MonadSplit classes.
-}

module Control.Monad.Random.Class (
    MonadRandom(..),
    MonadSplit(..),
    fromList,
    uniform,
    uniformMay
    ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Error
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.List
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy      as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict    as StrictRWS
import qualified Control.Monad.Trans.State.Lazy    as LazyState
import qualified Control.Monad.Trans.State.Strict  as StrictState
import qualified Control.Monad.Trans.Writer.Lazy   as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import           Data.Monoid
import           System.Random

-- | With a source of random number supply in hand, the 'MonadRandom' class
-- allows the programmer to extract random values of a variety of types.
class (Monad m) => MonadRandom m where
  -- | Takes a range /(lo,hi)/ and a random number generator
  -- /g/, and returns a computation that returns a random value uniformly
  -- distributed in the closed interval /[lo,hi]/, together with a new
  -- generator. It is unspecified what happens if /lo>hi/. For continuous
  -- types there is no requirement that the values /lo/ and /hi/ are ever
  -- produced, but they may be, depending on the implementation and the
  -- interval.
  --
  -- See 'System.Random.randomR' for details.
  getRandomR :: (Random a) => (a, a) -> m a

  -- | The same as 'getRandomR', but using a default range determined by the type:
  --
  -- * For bounded types (instances of 'Bounded', such as 'Char'),
  --   the range is normally the whole type.
  --
  -- * For fractional types, the range is normally the semi-closed interval
  -- @[0,1)@.
  --
  -- * For 'Integer', the range is (arbitrarily) the range of 'Int'.
  --
  -- See 'System.Random.random' for details.
  getRandom :: (Random a) => m a

  -- | Plural variant of 'getRandomR', producing an infinite list of
  -- random values instead of returning a new generator.
  --
  -- See 'System.Random.randomRs' for details.
  getRandomRs :: (Random a) => (a, a) -> m [a]

  -- | Plural variant of 'getRandom', producing an infinite list of
  -- random values instead of returning a new generator.
  --
  -- See 'System.Random.randoms' for details.
  getRandoms :: (Random a) => m [a]

instance MonadRandom IO where
  getRandomR = randomRIO
  getRandom = randomIO
  getRandomRs lohi = liftM (randomRs lohi) newStdGen
  getRandoms = liftM randoms newStdGen

instance (MonadRandom m) => MonadRandom (ContT r m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (Error e, MonadRandom m) => MonadRandom (ErrorT e m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (MonadRandom m) => MonadRandom (ExceptT e m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (MonadRandom m) => MonadRandom (IdentityT m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (MonadRandom m) => MonadRandom (ListT m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (MonadRandom m) => MonadRandom (MaybeT m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (Monoid w, MonadRandom m) => MonadRandom (LazyRWS.RWST r w s m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (Monoid w, MonadRandom m) => MonadRandom (StrictRWS.RWST r w s m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (MonadRandom m) => MonadRandom (ReaderT r m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (MonadRandom m) => MonadRandom (LazyState.StateT s m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (MonadRandom m) => MonadRandom (StrictState.StateT s m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (MonadRandom m, Monoid w) => MonadRandom (LazyWriter.WriterT w m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

instance (MonadRandom m, Monoid w) => MonadRandom (StrictWriter.WriterT w m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

-- | The class 'MonadSplit' proivides a way to specify a random number
-- generator that can be split into two new generators.
class (Monad m) => MonadSplit g m | m -> g where
  -- | The 'getSplit' operation allows one to obtain two distinct random number
  -- generators.
  --
  -- See 'System.Random.split' for details.
  getSplit :: m g

instance MonadSplit StdGen IO where
  getSplit = newStdGen

instance (MonadSplit g m) => MonadSplit g (ContT r m) where
  getSplit = lift getSplit

instance (Error e, MonadSplit g m) => MonadSplit g (ErrorT e m) where
  getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (ExceptT e m) where
  getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (IdentityT m) where
  getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (ListT m) where
  getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (MaybeT m) where
  getSplit = lift getSplit

instance (Monoid w, MonadSplit g m) => MonadSplit g (LazyRWS.RWST r w s m) where
  getSplit = lift getSplit

instance (Monoid w, MonadSplit g m) => MonadSplit g (StrictRWS.RWST r w s m) where
  getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (ReaderT r m) where
  getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (LazyState.StateT s m) where
  getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (StrictState.StateT s m) where
  getSplit = lift getSplit

instance (Monoid w, MonadSplit g m) => MonadSplit g (LazyWriter.WriterT w m) where
  getSplit = lift getSplit

instance (Monoid w, MonadSplit g m) => MonadSplit g (StrictWriter.WriterT w m) where
  getSplit = lift getSplit

-- | Sample a random value from a weighted list.  The list must be non-empty
-- and the total weight of all elements must be non-zero.
fromList :: (MonadRandom m) => [(a, Rational)] -> m a
fromList [] = do
  error "Control.Monad.Random.Class.fromList: empty list"
fromList [(x, _)] = do
  return x
fromList xs = do
  case fromRational (sum (map snd xs)) :: Double of
    0 -> do
      error "Control.Monad.Random.Class.fromList: total weight of all elements is zero"
    s -> do
      p <- liftM toRational $ getRandomR (0, s)
      return . fst . head . dropWhile ((< p) . snd) . scanl1 (\ ~(_x, old_s) ~(y, new_s) -> (y, old_s + new_s)) $ xs

-- | Sample a value from a uniform distribution of a list of elements.
uniform :: (MonadRandom m) => [a] -> m a
uniform = fromList . map (flip (,) 1)

-- | Sample a value from a uniform distribution of a list of elements
--   if that list is not empty.
uniformMay :: (MonadRandom m) => [a] -> m (Maybe a)
uniformMay [] = return Nothing
uniformMay xs = liftM Just (uniform xs)
