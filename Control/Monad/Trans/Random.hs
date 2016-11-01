{-# LANGUAGE Trustworthy #-}

{- |
Module       :  Control.Monad.Trans.Random
Copyright    :  (c) Cale Gibbard 2006-2007,
                (c) Russell O'Connor, Dan Doel and Remi Turk 2006,
                (c) Eric Kidd 2007
License      :  MIT-style (see the file LICENSE)

Maintainer   :  byorgey@gmail.com
Stability    :  experimental
Portability  :  non-portable (multi-param classes, functional dependencies, undecidable instances)

Random monads, passing a random number generator through a computation.

This version is lazy; for a strict version, see
"Control.Monad.Trans.Random.Strict", which has the same interface.
-}

module Control.Monad.Trans.Random
    ( module Control.Monad.Trans.Random.Lazy,
    ) where

import           Control.Monad.Trans.Random.Lazy
