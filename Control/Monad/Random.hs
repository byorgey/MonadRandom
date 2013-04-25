{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    Rand, RandT -- but not the data constructors
    -- * Example
    -- $RandExample
    ) where

import System.Random
import Control.Monad()
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Random.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans()
import Control.Monad.Writer
import Control.Arrow
import Control.Applicative

-- | A monad transformer which adds a random number generator to an
-- existing monad.
newtype RandT g m a = RandT (StateT g m a)
    deriving (Functor, Monad, MonadTrans, MonadIO, MonadFix)

instance (Functor m,Monad m) => Applicative (RandT g m) where
  pure = return
  (<*>) = ap

liftState :: (MonadState s m) => (s -> (a,s)) -> m a
liftState t = do v <- get
                 let (x, v') = t v
                 put v'
                 return x

instance (Monad m, RandomGen g) => MonadRandom (RandT g m) where
    getRandom = RandT . liftState $ random
    getRandoms = RandT . liftState $ first randoms . split
    getRandomR (x,y) = RandT . liftState $ randomR (x,y)
    getRandomRs (x,y) = RandT . liftState $
                            first (randomRs (x,y)) . split

instance (Monad m, RandomGen g) => MonadSplit g (RandT g m) where
    getSplit = RandT . liftState $ split

-- | Evaluate a RandT computation using the generator @g@.  Note that the
-- generator @g@ is not returned, so there's no way to recover the
-- updated version of @g@.
evalRandT :: (Monad m, RandomGen g) => RandT g m a -> g -> m a
evalRandT (RandT x) g = evalStateT x g

-- | Run a RandT computation using the generator @g@, returning the result and
-- the updated generator.
runRandT  :: (Monad m, RandomGen g) => RandT g m a -> g -> m (a, g)
runRandT (RandT x) g = runStateT x g

-- | A basic random monad.
newtype Rand g a = Rand (RandT g Identity a)
    deriving (Functor, Applicative, Monad, MonadRandom, MonadSplit g, MonadFix)

-- | Evaluate a random computation using the generator @g@.  Note that the
-- generator @g@ is not returned, so there's no way to recover the
-- updated version of @g@.
evalRand :: (RandomGen g) => Rand g a -> g -> a
evalRand (Rand x) g = runIdentity (evalRandT x g)

-- | Run a random computation using the generator @g@, returning the result
-- and the updated generator.
runRand :: (RandomGen g) => Rand g a -> g -> (a, g)
runRand (Rand x) g = runIdentity (runRandT x g)

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

instance (MonadRandom m) => MonadRandom (StateT s m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m, Monoid w) => MonadRandom (WriterT w m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m) => MonadRandom (ReaderT r m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (Error e, MonadRandom m) => MonadRandom (ErrorT e m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadSplit g m) => MonadSplit g (StateT s m) where
    getSplit = lift getSplit

instance (MonadSplit g m, Monoid w) => MonadSplit g (WriterT w m) where
    getSplit = lift getSplit

instance (MonadSplit g m) => MonadSplit g (ReaderT r m) where
    getSplit = lift getSplit

instance (Error e, MonadSplit g m) => MonadSplit g (ErrorT e m) where
    getSplit = lift getSplit

instance (MonadState s m, RandomGen g) => MonadState s (RandT g m) where
    get = lift get
    put = lift . put

instance (MonadReader r m, RandomGen g) => MonadReader r (RandT g m) where
    ask = lift ask
    local f (RandT m) = RandT $ local f m

instance (MonadWriter w m, RandomGen g, Monoid w) => MonadWriter w (RandT g m) where
    tell = lift . tell
    listen (RandT m) = RandT $ listen m
    pass (RandT m) = RandT $ pass m

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
