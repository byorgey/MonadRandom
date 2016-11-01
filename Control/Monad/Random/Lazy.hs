{-# LANGUAGE Trustworthy #-}

{- |
Module       :  Control.Monad.Random.Lazy
Copyright    :  (c) Cale Gibbard 2006-2007,
                (c) Russell O'Connor, Dan Doel and Remi Turk 2006,
                (c) Eric Kidd 2007
License      :  MIT-style (see the file LICENSE)

Maintainer   :  byorgey@gmail.com
Stability    :  experimental
Portability  :  non-portable (multi-param classes, functional dependencies, undecidable instances)

Lazy random monads.
-}

module Control.Monad.Random.Lazy
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
      evalRandTIO,
      -- * Lazy random monads
      module Control.Monad.Random.Class,
      module Control.Monad,
      module Control.Monad.Fix,
      module Control.Monad.Trans,
      module Data.Monoid,
    ) where

import           Control.Monad.Random.Class

import           Control.Monad.Trans
import           Control.Monad.Trans.Random.Lazy (Rand, RandT, evalRand,
                                                  evalRandIO, evalRandT,
                                                  evalRandTIO, execRand,
                                                  execRandT, liftRand,
                                                  liftRandT, mapRand, mapRandT,
                                                  runRand, runRandT, withRand,
                                                  withRandT)

import           Control.Monad
import           Control.Monad.Fix
import           Data.Monoid
