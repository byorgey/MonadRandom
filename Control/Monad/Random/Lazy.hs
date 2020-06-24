{-# LANGUAGE Safe #-}

{- |
Module       :  Control.Monad.Random.Lazy
Copyright    :  (c) Brent Yorgey 2016
License      :  BSD3 (see LICENSE)

Maintainer   :  byorgey@gmail.com
Stability    :  experimental
Portability  :  non-portable (multi-param classes, functional dependencies, undecidable instances)

Random monads that are lazy in the generator state. For a strict
version, see "Control.Monad.Random.Strict", which has the same
interface.
-}

module Control.Monad.Random.Lazy
    ( -- * The Rand monad
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
import           Control.Monad.Trans.Random.Lazy (Rand, RandT, evalRand,
                                                  evalRandIO, evalRandT,
                                                  evalRandTIO, execRand,
                                                  execRandT, liftRand,
                                                  liftRandT, mapRand, mapRandT,
                                                  runRand, runRandT, withRand,
                                                  withRandT)

import           Control.Monad
import           Control.Monad.Fix
