{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{- |
Module       :  Control.Monad.Trans.Random.Lazy
Copyright    :  (c) Brent Yorgey 2016
License      :  BSD3 (see LICENSE)

Maintainer   :  byorgey@gmail.com
Stability    :  experimental
Portability  :  non-portable (multi-param classes, functional dependencies, undecidable instances)

Lazy random monads, passing a random number generator through a computation.
See below for examples.

For a strict version with the same interface, see
"Control.Monad.Trans.Random.Strict".
-}

module Control.Monad.Trans.Random.Lazy
  ( -- * The Rand monad transformer
    Rand,
    liftRand,
    runRand,
    evalRand,
    execRand,
    mapRand,
    withRand,
    evalRandIO,
    -- * The RandT monad transformer
    RandT,
    liftRandT,
    runRandT,
    evalRandT,
    execRandT,
    mapRandT,
    withRandT,
    -- * Lifting other operations
    liftCallCC,
    liftCallCC',
    liftCatch,
    liftListen,
    liftPass,
    evalRandTIO,
    -- * Examples
    -- ** Random monads
    -- $examples
  ) where

import           Control.Applicative
import           Control.Arrow                  (first)
import           Control.Monad
import           Control.Monad.Cont.Class
import           Control.Monad.Error.Class
import qualified Control.Monad.Fail             as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Control.Monad.Random.Class
import           Control.Monad.RWS.Class
import           Control.Monad.Signatures
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Lazy as LazyState
import           Data.Functor.Identity
import           System.Random

-- | A random monad parameterized by the type @g@ of the generator to carry.
--
-- The 'return' function leaves the generator unchanged, while '>>=' uses the
-- final generator of the first computation as the initial generator of the
-- second.
type Rand g = RandT g Identity

-- | Construct a random monad computation from a function.
-- (The inverse of 'runRand'.)
liftRand
  :: (g -> (a, g))
  -- ^ pure random transformer
  -> Rand g a
  -- ^ equivalent generator-passing computation
liftRand = RandT . state

-- | Unwrap a random monad computation as a function.
-- (The inverse of 'liftRand'.)
runRand
  :: Rand g a
  -- ^ generator-passing computation to execute
  -> g
  -- ^ initial generator
  -> (a, g)
  -- ^ return value and final generator
runRand t = runIdentity . runRandT t

-- | Evaluate a random computation with the given initial generator and return
-- the final value, discarding the final generator.
--
-- * @'evalRand' m s = fst ('runRand' m s)@
evalRand
  :: Rand g a
  -- ^ generator-passing computation to execute
  -> g
  -- ^ initial generator
  -> a
  -- ^ return value of the random computation
evalRand t = runIdentity . evalRandT t

-- | Evaluate a random computation with the given initial generator and return
-- the final generator, discarding the final value.
--
-- * @'execRand' m s = snd ('runRand' m s)@
execRand
  :: Rand g a
  -- ^ generator-passing computation to execute
  -> g
  -- ^ initial generator
  -> g
  -- ^ final generator
execRand t = runIdentity . execRandT t

-- | Map both the return value and final generator of a computation using the
-- given function.
--
-- * @'runRand' ('mapRand' f m) = f . 'runRand' m@
mapRand :: ((a, g) -> (b, g)) -> Rand g a -> Rand g b
mapRand f = mapRandT (liftM f)

-- | @'withRand' f m@ executes action @m@ on a generator modified by applying @f@.
--
-- * @'withRand' f m = 'modify' f >> m@
withRand :: (g -> g) -> Rand g a -> Rand g a
withRand = withRandT

-- | A random transformer monad parameterized by:
--
-- * @g@ - The generator.
--
-- * @m@ - The inner monad.
--
-- The 'return' function leaves the generator unchanged, while '>>=' uses the
-- final generator of the first computation as the initial generator of the
-- second.
newtype RandT g m a = RandT { unRandT :: LazyState.StateT g m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans, MonadIO, MonadFix, MonadReader r, MonadWriter w)

-- | Construct a random monad computation from an impure function.
-- (The inverse of 'runRandT'.)
liftRandT
  :: (g -> m (a, g))
  -- ^ impure random transformer
  -> RandT g m a
  -- ^ equivalent generator-passing computation
liftRandT = RandT . LazyState.StateT

-- | Unwrap a random monad computation as an impure function.
-- (The inverse of 'liftRandT'.)
runRandT
  :: RandT g m a
  -- ^ generator-passing computation to execute
  -> g
  -- ^ initial generator
  -> m (a, g)
  -- ^ return value and final generator
runRandT = LazyState.runStateT . unRandT

-- | Evaluate a random computation with the given initial generator and return
-- the final value, discarding the final generator.
--
-- * @'evalRandT' m g = liftM fst ('runRandT' m g)@
evalRandT :: (Monad m) => RandT g m a -> g -> m a
evalRandT = LazyState.evalStateT . unRandT

-- | Evaluate a random computation with the given initial generator and return
-- the final generator, discarding the final value.
--
-- * @'execRandT' m g = liftM snd ('runRandT' m g)@
execRandT :: (Monad m) => RandT g m a -> g -> m g
execRandT = LazyState.execStateT . unRandT

-- | Map both the return value and final generator of a computation using the
-- given function.
--
-- * @'runRandT' ('mapRandT' f m) = f . 'runRandT' m@
mapRandT :: (m (a, g) -> n (b, g)) -> RandT g m a -> RandT g n b
mapRandT f = RandT . LazyState.mapStateT f . unRandT

-- | @'withRandT' f m@ executes action @m@ on a generator modified by applying @f@.
--
-- * @'withRandT' f m = 'modify' f >> m@
withRandT :: (g -> g) -> RandT g m a -> RandT g m a
withRandT f = RandT . LazyState.withStateT f . unRandT

instance (MonadCont m) => MonadCont (RandT g m) where
  callCC = liftCallCC' callCC

instance (MonadError e m) => MonadError e (RandT g m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

instance (MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (RandT g m)

instance (RandomGen g, Monad m) => MonadRandom (RandT g m) where
  getRandomR lohi = RandT . state $ randomR lohi
  getRandom = RandT . state $ random
  getRandomRs lohi = RandT . state $ first (randomRs lohi) . split
  getRandoms = RandT . state $ first randoms . split

instance (RandomGen g, Monad m) => MonadSplit g (RandT g m) where
  getSplit = RandT . state $ split

instance (Monad m, RandomGen g) => MonadInterleave (RandT g m) where
  interleave (RandT m) = liftRandT $ \g -> case split g of
    (gl, gr) -> liftM (\p -> (fst p, gr)) $ LazyState.runStateT m gl

instance (MonadState s m) => MonadState s (RandT g m) where
  get = lift get
  put = lift . put

instance PrimMonad m => PrimMonad (RandT s m) where
  type PrimState (RandT s m) = PrimState m
  primitive = lift . primitive

instance Fail.MonadFail m => Fail.MonadFail (RandT g m) where
  fail = lift . Fail.fail

-- | Uniform lifting of a @callCC@ operation to the new monad.
-- This version rolls back to the original state on entering the
-- continuation.
liftCallCC :: CallCC m (a, g) (b, g) -> CallCC (RandT g m) a b
liftCallCC callCC_ f = RandT $ LazyState.liftCallCC callCC_ $ \c -> unRandT (f (RandT . c))

-- | In-situ lifting of a @callCC@ operation to the new monad.
-- This version uses the current state on entering the continuation.
-- It does not satisfy the uniformity property (see "Control.Monad.Signatures").
liftCallCC' :: CallCC m (a, g) (b, g) -> CallCC (RandT g m) a b
liftCallCC' callCC_ f = RandT $ LazyState.liftCallCC' callCC_ $ \c -> unRandT (f (RandT . c))

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (a, g) -> Catch e (RandT g m) a
liftCatch catchE_ m f = RandT $ LazyState.liftCatch catchE_ (unRandT m) (unRandT . f)

-- | Lift a @listen@ operation to the new monad.
liftListen :: (Monad m) => Listen w m (a, g) -> Listen w (RandT g m) a
liftListen listen_ m = RandT $ LazyState.liftListen listen_ (unRandT m)

-- | Lift a @pass@ operation to the new monad.
liftPass :: (Monad m) => Pass w m (a, g) -> Pass w (RandT g m) a
liftPass pass_ m = RandT $ LazyState.liftPass pass_ (unRandT m)

-- | Evaluate a random computation in the `IO` monad, splitting the global
-- standard generator to get a new one for the computation.
evalRandIO :: Rand StdGen a -> IO a
evalRandIO t = liftM (evalRand t) newStdGen

-- | Evaluate a random computation that is embedded in the `IO` monad,
-- splitting the global standard generator to get a new one for the
-- computation.
evalRandTIO :: (MonadIO m) => RandT StdGen m a -> m a
evalRandTIO t = liftIO newStdGen >>= evalRandT t

{- $examples

The @die@ function simulates the roll of a die, picking a number between 1
and 6, inclusive, and returning it in the 'Rand' monad transformer.  Notice
that this code will work with any random number generator @g@.

> die :: (RandomGen g) => Rand g Int
> die = getRandomR (1, 6)

The @dice@ function uses @replicate@ and @sequence@ to simulate the roll of
@n@ dice.

> dice :: (RandomGen g) => Int -> Rand g [Int]
> dice n = sequence (replicate n die)

To extract a value from the 'Rand' monad transformer, we can use 'evalRandIO'.

> main = do
>   values <- evalRandIO (dice 2)
>   putStrLn (show values)

-}
