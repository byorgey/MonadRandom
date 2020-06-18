{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Safe                   #-}
{-# LANGUAGE UndecidableInstances   #-}

{- |
Module       :  Control.Monad.Random.Class
Copyright    :  (c) Brent Yorgey 2016
License      :  BSD3 (see LICENSE)
Maintainer   :  byorgey@gmail.com

The 'MonadRandom', 'MonadSplit', and 'MonadInterleave' classes.

* 'MonadRandom' abstracts over monads with the capability of
  generating random values.

* 'MonadSplit' abstracts over random monads with the ability to get a
  split generator state.  It is not very useful but kept here for
  backwards compatibility.

* 'MonadInterleave' abstracts over random monads supporting an
  'interleave' operation, which allows sequencing computations which do
  not depend on each other's random generator state, by splitting the
  generator between them.

This module also defines convenience functions for sampling from a
given collection of values, either uniformly or according to given
weights.

-}

module Control.Monad.Random.Class (

    -- * MonadRandom
    MonadRandom(..),

    -- * MonadSplit
    MonadSplit(..),

    -- * MonadInterleave
    MonadInterleave(..),

    -- * Sampling functions
    fromList,
    fromListMay,
    uniform,
    uniformMay,
    weighted,
    weightedMay
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
import qualified System.Random                     as Random

import qualified Data.Foldable                     as F

#if MIN_VERSION_base(4,8,0)
#else
import           Data.Monoid                       (Monoid)
#endif

------------------------------------------------------------
-- MonadRandom
------------------------------------------------------------

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
  getRandomR :: (Random.Random a) => (a, a) -> m a

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
  getRandom :: (Random.Random a) => m a

  -- | Plural variant of 'getRandomR', producing an infinite list of
  -- random values instead of returning a new generator.
  --
  -- See 'System.Random.randomRs' for details.
  getRandomRs :: (Random.Random a) => (a, a) -> m [a]

  -- | Plural variant of 'getRandom', producing an infinite list of
  -- random values instead of returning a new generator.
  --
  -- See 'System.Random.randoms' for details.
  getRandoms :: (Random.Random a) => m [a]

instance MonadRandom IO where
  getRandomR       = Random.randomRIO
  getRandom        = Random.randomIO
  getRandomRs lohi = liftM (Random.randomRs lohi) Random.newStdGen
  getRandoms       = liftM Random.randoms Random.newStdGen

instance (MonadRandom m) => MonadRandom (ContT r m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (Error e, MonadRandom m) => MonadRandom (ErrorT e m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (MonadRandom m) => MonadRandom (ExceptT e m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (MonadRandom m) => MonadRandom (IdentityT m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (MonadRandom m) => MonadRandom (ListT m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (MonadRandom m) => MonadRandom (MaybeT m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (Monoid w, MonadRandom m) => MonadRandom (LazyRWS.RWST r w s m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (Monoid w, MonadRandom m) => MonadRandom (StrictRWS.RWST r w s m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (MonadRandom m) => MonadRandom (ReaderT r m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (MonadRandom m) => MonadRandom (LazyState.StateT s m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (MonadRandom m) => MonadRandom (StrictState.StateT s m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (MonadRandom m, Monoid w) => MonadRandom (LazyWriter.WriterT w m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

instance (MonadRandom m, Monoid w) => MonadRandom (StrictWriter.WriterT w m) where
  getRandomR  = lift . getRandomR
  getRandom   = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms  = lift getRandoms

------------------------------------------------------------
-- MonadSplit
------------------------------------------------------------

-- | The class 'MonadSplit' proivides a way to specify a random number
--   generator that can be split into two new generators.
--
--   This class is not very useful in practice: typically, one cannot
--   actually do anything with a generator.  It remains here to avoid
--   breaking existing code unnecessarily.  For a more practically
--   useful interface, see 'MonadInterleave'.
class (Monad m) => MonadSplit g m | m -> g where

  -- | The 'getSplit' operation allows one to obtain two distinct random number
  -- generators.
  --
  -- See 'System.Random.split' for details.
  getSplit :: m g

instance MonadSplit Random.StdGen IO where
  getSplit = Random.newStdGen

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

------------------------------------------------------------
-- MonadInterleave
------------------------------------------------------------

-- | The class 'MonadInterleave' proivides a convenient interface atop
--   a 'split' operation on a random generator.
class MonadRandom m => MonadInterleave m where

  -- | If @x :: m a@ is a computation in some random monad, then
  --   @interleave x@ works by splitting the generator, running @x@
  --   using one half, and using the other half as the final generator
  --   state of @interleave x@ (replacing whatever the final generator
  --   state otherwise would have been).  This means that computation
  --   needing random values which comes after @interleave x@ does not
  --   necessarily depend on the computation of @x@.  For example:
  --
  --   > >>> evalRandIO $ snd <$> ((,) <$> undefined <*> getRandom)
  --   > *** Exception: Prelude.undefined
  --   > >>> evalRandIO $ snd <$> ((,) <$> interleave undefined <*> getRandom)
  --   > 6192322188769041625
  --
  --   This can be used, for example, to allow random computations to
  --   run in parallel, or to create lazy infinite structures of
  --   random values.  In the example below, the infinite tree
  --   @randTree@ cannot be evaluated lazily: even though it is cut
  --   off at two levels deep by @hew 2@, the random value in the
  --   right subtree still depends on generation of all the random
  --   values in the (infinite) left subtree, even though they are
  --   ultimately unneeded.  Inserting a call to @interleave@, as in
  --   @randTreeI@, solves the problem: the generator splits at each
  --   @Node@, so random values in the left and right subtrees are
  --   generated independently.
  --
  --   > data Tree = Leaf | Node Int Tree Tree deriving Show
  --   >
  --   > hew :: Int -> Tree -> Tree
  --   > hew 0 _    = Leaf
  --   > hew _ Leaf = Leaf
  --   > hew n (Node x l r) = Node x (hew (n-1) l) (hew (n-1) r)
  --   >
  --   > randTree :: Rand StdGen Tree
  --   > randTree = Node <$> getRandom <*> randTree <*> randTree
  --   >
  --   > randTreeI :: Rand StdGen Tree
  --   > randTreeI = interleave $ Node <$> getRandom <*> randTreeI <*> randTreeI
  --
  --   > >>> hew 2 <$> evalRandIO randTree
  --   > Node 2168685089479838995 (Node (-1040559818952481847) Leaf Leaf) (Node ^CInterrupted.
  --   > >>> hew 2 <$> evalRandIO randTreeI
  --   > Node 8243316398511136358 (Node 4139784028141790719 Leaf Leaf) (Node 4473998613878251948 Leaf Leaf)
  interleave :: m a -> m a

instance (MonadInterleave m) => MonadInterleave (ContT r m) where
  interleave = mapContT interleave

instance (Error e, MonadInterleave m) => MonadInterleave (ErrorT e m) where
  interleave = mapErrorT interleave

instance (MonadInterleave m) => MonadInterleave (ExceptT e m) where
  interleave = mapExceptT interleave

instance (MonadInterleave m) => MonadInterleave (IdentityT m) where
  interleave = mapIdentityT interleave

instance (MonadInterleave m) => MonadInterleave (ListT m) where
  interleave = mapListT interleave

instance (MonadInterleave m) => MonadInterleave (MaybeT m) where
  interleave = mapMaybeT interleave

instance (Monoid w, MonadInterleave m) => MonadInterleave (LazyRWS.RWST r w s m) where
  interleave = LazyRWS.mapRWST interleave

instance (Monoid w, MonadInterleave m) => MonadInterleave (StrictRWS.RWST r w s m) where
  interleave = StrictRWS.mapRWST interleave

instance (MonadInterleave m) => MonadInterleave (ReaderT r m) where
  interleave = mapReaderT interleave

instance (MonadInterleave m) => MonadInterleave (LazyState.StateT s m) where
  interleave = LazyState.mapStateT interleave

instance (MonadInterleave m) => MonadInterleave (StrictState.StateT s m) where
  interleave = StrictState.mapStateT interleave

instance (Monoid w, MonadInterleave m) => MonadInterleave (LazyWriter.WriterT w m) where
  interleave = LazyWriter.mapWriterT interleave

instance (Monoid w, MonadInterleave m) => MonadInterleave (StrictWriter.WriterT w m) where
  interleave = StrictWriter.mapWriterT interleave

------------------------------------------------------------
-- Convenience samplers
------------------------------------------------------------

-- | Sample a random value from a weighted nonempty collection of
--   elements.  Crashes with a call to @error@ if the collection is
--   empty or the total weight is zero.
weighted :: (F.Foldable t, MonadRandom m) => t (a, Rational) -> m a
weighted t = do
  ma <- weightedMay t
  case ma of
    Nothing -> error "Control.Monad.Random.Class.weighted: empty collection, or total weight <= 0"
    Just a  -> return a

-- | Sample a random value from a weighted collection of elements.
--   Returns @Nothing@ if the collection is empty or the total weight is
--   zero.
weightedMay :: (F.Foldable t, MonadRandom m) => t (a, Rational) -> m (Maybe a)
weightedMay = fromListMay . F.toList

-- | Sample a random value from a weighted list.  The list must be
--   non-empty and the total weight must be non-zero.
fromList :: (MonadRandom m) => [(a, Rational)] -> m a
fromList ws = do
  ma <- fromListMay ws
  case ma of
    Nothing -> error "Control.Monad.Random.Class.fromList: empty list, or total weight = 0"
    Just a  -> return a

-- | Sample a random value from a weighted list.  Return @Nothing@ if
--   the list is empty or the total weight is nonpositive.
fromListMay :: (MonadRandom m) => [(a, Rational)] -> m (Maybe a)
fromListMay xs = do
  let s    = fromRational (sum (map snd xs)) :: Double
      cums = scanl1 (\ ~(_,q) ~(y,s') -> (y, s'+q)) xs
  case s <= 0 of
    True -> return Nothing
    _    -> do
      p <- liftM toRational $ getRandomR (0, s)
      return . Just . fst . head . dropWhile ((< p) . snd) $ cums

-- | Sample a value uniformly from a nonempty collection of elements.
uniform :: (F.Foldable t, MonadRandom m) => t a -> m a
uniform t = do
  ma <- uniformMay t
  case ma of
    Nothing -> error "Control.Monad.Random.Class.uniform: empty collection"
    Just a  -> return a

-- | Sample a value uniformly from a collection of elements.  Return
--   @Nothing@ if the collection is empty.
uniformMay :: (F.Foldable t, MonadRandom m) => t a -> m (Maybe a)
uniformMay = fromListMay . map (flip (,) 1) . F.toList
