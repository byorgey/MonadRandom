{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-} 
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

{- |
Copyright    : 2006-2007 Cale Gibbard, Russell O'Connor, Dan Doel, Remi Turk, Eric Kidd.
License      : OtherLicense
Stability    : experimental
Portability  : non-portable (multi-parameter type classes, undecidable instances)

A type class for random number generation monads.  See
<http://www.haskell.org/haskellwiki/NewMonads/MonadRandom> for the original
version of this code.

Instances of this type class include 'Control.Monad.Random.Rand' and
monads created using 'Control.Monad.Random.RandT'.

-}

module Control.Monad.Random.Class (
    MonadRandom,
    getRandom,
    getRandomR,
    getRandoms,
    getRandomRs,
    MonadSplit,
    getSplit
    ) where

import System.Random
import           Control.Monad.Cont
import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.Trans          ()
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.RWS.Lazy       as RWSL
import qualified Control.Monad.RWS.Strict     as RWSS
import qualified Control.Monad.State.Lazy     as SL
import qualified Control.Monad.State.Strict   as SS
import qualified Control.Monad.Writer.Lazy    as WL
import qualified Control.Monad.Writer.Strict  as WS

-- | An interface to random number generation monads.
class (Monad m) => MonadRandom m where
    -- | Return a randomly-selected value of type @a@.  See
    -- 'System.Random.random' for details.
    getRandom :: (Random a) => m a
    -- | Return an infinite stream of random values of type @a@.  See
    -- 'System.Random.randoms' for details.
    getRandoms :: (Random a) => m [a]
    -- | Return a randomly-selected value of type @a@ in the range
    -- /[lo,hi]/.  See 'System.Random.randomR' for details.
    getRandomR :: (Random a) => (a,a) -> m a
    -- | Return an infinite stream of randomly-selected value of type @a@
    -- in the range /[lo,hi]/.  See 'System.Random.randomRs' for details.
    getRandomRs :: (Random a) => (a,a) -> m [a]

-- | An interface to monads with splittable state (as most random number generation monads will have).
-- The intention is that the 'getSplit' action splits the state, returning one half of the result, and
-- setting the new state to the other.
class (Monad m) => MonadSplit s m | m -> s where
    getSplit :: m s

instance (MonadRandom m) => MonadRandom (IdentityT m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m) => MonadRandom (SL.StateT s m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m) => MonadRandom (SS.StateT s m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m, Monoid w) => MonadRandom (WL.WriterT w m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m, Monoid w) => MonadRandom (WS.WriterT w m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m) => MonadRandom (ReaderT r m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m, Monoid w) => MonadRandom (RWSL.RWST r w s m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m, Monoid w) => MonadRandom (RWSS.RWST r w s m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m) => MonadRandom (ExceptT e m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (Error e, MonadRandom m) => MonadRandom (ErrorT e m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m) => MonadRandom (MaybeT m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance MonadRandom m => MonadRandom (ContT r m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadSplit g m) => MonadSplit g (IdentityT m) where
    getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (SL.StateT s m) where
    getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (SS.StateT s m) where
    getSplit = lift getSplit

instance (MonadSplit g m, Monoid w) => MonadSplit g (WL.WriterT w m) where
    getSplit = lift getSplit

instance (MonadSplit g m, Monoid w) => MonadSplit g (WS.WriterT w m) where
    getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (ReaderT r m) where
    getSplit = lift getSplit

instance (MonadSplit g m, Monoid w) => MonadSplit g (RWSL.RWST r w s m) where
    getSplit = lift getSplit

instance (MonadSplit g m, Monoid w) => MonadSplit g (RWSS.RWST r w s m) where
    getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (ExceptT e m) where
    getSplit = lift getSplit

instance (Error e, MonadSplit g m) => MonadSplit g (ErrorT e m) where
    getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (MaybeT m) where
    getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (ContT r m) where
    getSplit = lift getSplit

instance MonadRandom IO where
    getRandom = randomIO
    getRandomR = randomRIO
    getRandoms = fmap randoms newStdGen
    getRandomRs b = fmap (randomRs b) newStdGen

instance MonadSplit StdGen IO where
    getSplit = newStdGen

