{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -fno-warn-orphans #-}

{- |
Copyright    : 2006-2007 Cale Gibbard, Russell O'Connor, Dan Doel, Remi Turk, Eric Kidd.
License      : OtherLicense
Stability    : experimental
Portability  : non-portable (multi-parameter type classes, undecidable instances)

A random number generation monad.  See
<http://www.haskell.org/haskellwiki/NewMonads/MonadRandom> for the original
version of this code.

The actual interface is defined by
'Control.Monad.Random.Class.MonadRandom'.

[Computation type:] Computations which consume random values.

[Binding strategy:] The computation proceeds in the same fashion as the
identity monad, but it carries a random number generator that may be
queried to generate random values.

[Useful for:] Monte Carlo algorithms and simulating random processes.

-}

module Control.Monad.Random (
    module System.Random,
    module Control.Monad.Random.Class,
    evalRandT,
    runRandT,
    evalRand,
    runRand,
    evalRandIO,
    fromList,
    uniform,
    Rand, RandT, -- but not the data constructors
    -- * Special lift functions
    liftRand,
    liftRandT
    -- * Example
    -- $RandExample
    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad                ()
import           Control.Monad.Cont
import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.Random.Class
import           Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy       as RWSL
import qualified Control.Monad.RWS.Strict     as RWSS
import           Control.Monad.State
import qualified Control.Monad.State.Lazy     as SL
import qualified Control.Monad.State.Strict   as SS
import           Control.Monad.Trans          ()
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Class
import qualified Control.Monad.Writer.Lazy    as WL
import qualified Control.Monad.Writer.Strict  as WS
import           Data.Monoid                  (Monoid)
import           System.Random

-- | A monad transformer which adds a random number generator to an
-- existing monad.
newtype RandT g m a = RandT (StateT g m a)
    deriving (Functor, Monad, MonadPlus, MonadTrans, MonadIO, MonadFix, MonadReader r, MonadWriter w)

instance (Functor m, Monad m) => Applicative (RandT g m) where
  pure = return
  (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (RandT g m) where
  empty = mzero
  (<|>) = mplus

-- | Lift arbitrary action to RandT
liftRandT :: (g -> m (a, g)) -- ^ action returning value and new generator state
             -> RandT g m a
liftRandT = RandT . StateT

-- | Lift arbitrary action to Rand
liftRand :: (g -> (a, g)) -- ^ action returning value and new generator state
            -> Rand g a
liftRand = RandT . state

instance (Monad m, RandomGen g) => MonadRandom (RandT g m) where
    getRandom = RandT . state $ random
    getRandoms = RandT . state $ first randoms . split
    getRandomR (x,y) = RandT . state $ randomR (x,y)
    getRandomRs (x,y) = RandT . state $
                            first (randomRs (x,y)) . split

instance (Monad m, RandomGen g) => MonadSplit g (RandT g m) where
    getSplit = RandT . state $ split

-- | Evaluate a RandT computation using the generator @g@.  Note that the
-- generator @g@ is not returned, so there's no way to recover the
-- updated version of @g@.
evalRandT :: (Monad m) => RandT g m a -> g -> m a
evalRandT (RandT x) g = evalStateT x g

-- | Run a RandT computation using the generator @g@, returning the result and
-- the updated generator.
runRandT  :: RandT g m a -> g -> m (a, g)
runRandT (RandT x) g = runStateT x g

-- | A basic random monad.
type Rand g = RandT g Identity

-- | Evaluate a random computation using the generator @g@.  Note that the
-- generator @g@ is not returned, so there's no way to recover the
-- updated version of @g@.
evalRand :: Rand g a -> g -> a
evalRand x g = runIdentity (evalRandT x g)

-- | Run a random computation using the generator @g@, returning the result
-- and the updated generator.
runRand :: Rand g a -> g -> (a, g)
runRand x g = runIdentity (runRandT x g)

-- | Evaluate a random computation in the IO monad, splitting the global standard generator to get a new one for the computation.
evalRandIO :: Rand StdGen a -> IO a
evalRandIO x = fmap (evalRand x) newStdGen

-- | Sample a random value from a weighted list.  The total weight of all
-- elements must not be 0.
fromList :: (MonadRandom m) => [(a,Rational)] -> m a
fromList [] = error "MonadRandom.fromList called with empty list"
fromList [(x,_)] = return x
fromList xs = do
  -- TODO: Do we want to be able to use floats as weights?
  -- TODO: Better error message if weights sum to 0.
  let s = (fromRational (sum (map snd xs))) :: Double -- total weight
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs       -- cumulative weight
  p <- liftM toRational $ getRandomR (0.0,s)
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs

-- | Sample a value from a uniform distribution of a list of elements.
uniform :: (MonadRandom m) => [a] -> m a
uniform = fromList . fmap (flip (,) 1)

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

instance (MonadState s m) => MonadState s (RandT g m) where
    get = lift get
    put = lift . put

instance MonadRandom IO where
    getRandom = randomIO
    getRandomR = randomRIO
    getRandoms = fmap randoms newStdGen
    getRandomRs b = fmap (randomRs b) newStdGen

instance MonadSplit StdGen IO where
    getSplit = newStdGen

{- $RandExample

The @die@ function simulates the roll of a die, picking a number between 1
and 6, inclusive, and returning it in the 'Rand' monad.  Notice that this
code will work with any source of random numbers @g@.

>die :: (RandomGen g) => Rand g Int
>die = getRandomR (1,6)

The @dice@ function uses @replicate@ and @sequence@ to simulate the roll of
@n@ dice.

>dice :: (RandomGen g) => Int -> Rand g [Int]
>dice n = sequence (replicate n die)

To extract a value from the 'Rand' monad, we can can use 'evalRandIO'.

>main = do
>  values <- evalRandIO (dice 2)
>  putStrLn (show values)

-}
