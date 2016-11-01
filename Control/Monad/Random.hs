{-# LANGUAGE Safe #-}

{- |
Module       :  Control.Monad.Random
Copyright    :  (c) Cale Gibbard 2006-2007,
                (c) Russell O'Connor, Dan Doel and Remi Turk 2006,
                (c) Eric Kidd 2007
License      :  MIT-style (see the file LICENSE)

Maintainer   :  byorgey@gmail.com
Stability    :  experimental
Portability  :  non-portable (multi-param classes, functional dependencies, undecidable instances)

Declaration of MonadRandom and MonadSplit classes.
-}

module Control.Monad.Random
    ( module Control.Monad.Random.Lazy,
    ) where

import           Control.Monad.Random.Lazy
