{-# LANGUAGE Safe #-}

{- |
Module       :  Control.Monad.Random
Copyright    :  (c) Brent Yorgey 2016
License      :  BSD3 (see LICENSE)

Maintainer   :  byorgey@gmail.com
Stability    :  experimental
Portability  :  non-portable (multi-param classes, functional dependencies, undecidable instances)

This module is provided for backwards compatibility, and simply
re-exports "Control.Monad.Random.Lazy".
-}

module Control.Monad.Random
    ( module Control.Monad.Random.Lazy,
    ) where

import           Control.Monad.Random.Lazy
