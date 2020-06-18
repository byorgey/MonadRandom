{-# LANGUAGE Safe #-}

{- |
Module       :  Control.Monad.Random.Strict
Copyright    :  (c) Brent Yorgey 2016
License      :  BSD3 (see LICENSE)

Maintainer   :  byorgey@gmail.com
Stability    :  experimental
Portability  :  non-portable (multi-param classes, functional dependencies, undecidable instances)

Random monads that are strict in the generator state.  For a lazy
version, see "Control.Monad.Random.Lazy", which has the same
interface.
-}

module Control.Monad.Random.Strict
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
      -- * Some convenience re-exports

      module System.Random,
      module Control.Monad.Random.Class,
      module Control.Monad,
      module Control.Monad.Fix,
      module Control.Monad.Trans,
    ) where

import           System.Random hiding (uniform, uniformR)

import           Control.Monad.Random.Class

import           Control.Monad.Trans
import           Control.Monad.Trans.Random.Strict (Rand, RandT, evalRand,
                                                    evalRandIO, evalRandT,
                                                    evalRandTIO, execRand,
                                                    execRandT, liftRand,
                                                    liftRandT, mapRand,
                                                    mapRandT, runRand, runRandT,
                                                    withRand, withRandT)

import           Control.Monad
import           Control.Monad.Fix
