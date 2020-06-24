0.5.2 (24 June 2020)
--------------------

- Support for `random-1.2`.
- Drop support for GHC 7.4, test with GHC 8.10.

0.5.1.2 (18 Jan 2020)
---------------------

- Better error message when total probability sum is negative in
  fromListMay, weightedMay, weighted

0.5.1.1 (21 May 2018)
---------------------

- Only depend on `fail` package when GHC < 8.0.

0.5.1 (9 February 2017)
-----------------------

Re-export `System.Random` from `Control.Monad.Random.{Lazy,Strict}`
and hence also from `Control.Monad.Random`.

- Hackage r1: allow `transformers-compat-0.6.x`.

0.5 (3 January 2017)
--------------------

  This release has quite a few small additions as well as a big module
  reorganization.  However, thanks to module re-exports, most existing
  code using the library should continue to work with no changes; the
  major version bump reflects the large reorganization and my
  inability to 100% guarantee that existing user code will not break.

  The biggest changes that may be of interest to users of the library
  include new lazy vs strict variants of the `Rand` monad; a new
  `MonadInterleave` class which is a big improvement over
  `MonadSplit`; new `PrimMonad` instances; and new random selection
  functions like `weighted`, `weightedMay`, `uniformMay`, *etc.*.  See
  the list below for full details.

  Although there was some discussion of generalizing `MonadRandom` to
  work for a wider range of underlying generators
  (see
  [#26](https://github.com/byorgey/MonadRandom/issues/26),
  [#31](https://github.com/byorgey/MonadRandom/issues/31), and
  [comments on this blog post](https://byorgey.wordpress.com/2016/11/16/monadrandom-0-5-and-mwc-random-feedback-wanted/)),
  I decided to punt on that for now. It seems rather complicated and
  there
  are
  [already good alternatives](http://hackage.haskell.org/package/random%2Dfu) so
  I decided to keep things simple for this release.  I'm still open to
  proposals for generalizing future releases.

  Changes in 0.5 include:

  - Refactor to reflect structure of `mtl` and `transformers` libraries.
  - Add lazy and strict variants of `RandT`.
  - Add `MonadRandom` and `MonadSplit` instances for `ListT`.
  - Add (but do not export) `unRandT` field to `RandT`.
  - Add `MonadCont`, `MonadError`, `MonadRWS`, `PrimMonad`, and `MonadFail`
    instances for `RandT`.
  - Add `evalRandTIO` operation.
  - Move `fromList` and `uniform` operations to
    `Control.Monad.Random.Class`.
  - `fromList` now raises an error when the total weight of elements
    is zero.
  - Generalize the type of `uniform` to work over any `Foldable`.
  - Add new operations `weighted`, `weightedMay`, `fromListMay`, and
    `uniformMay`.  `weighted` is like `fromList` but generalized to
    work over any `Foldable`.  The `May` variants return a `Maybe`
    result instead of raising an error.
  - New `MonadInterleave` class for random monads which can interleave
    random generation using `split`.  In some ways this is similar to
    `MonadSplit` but much more useful.
  - Improved documentation.

0.4.2.3 (21 April 2016)
-----------------------

  - Mark `Control.Monad.Random` as `Trustworthy`.

0.4.2.2 (18 January 2016)
-------------------------

  - Allow `transformers-0.5`.

0.4.2.1 (16 January 2016)
-------------------------

  - Allow `transformers-compat-0.5`.

0.4.2 (16 January 2016)
-----------------------

  - Add `MonadPlus` and `Alternative` instances for `RandT`.

0.4.1 (20 November 2015)
------------------------

  - Remove unnecessary `Monad m` constraint from `liftRandT` and
    `runRandT`.

  This should again technically require a major version bump, but I'm
  not doing it this time in the interest of not being super annoying.
  If this breaks something for you, just yell, and I will
  deprecate this version and do a proper 0.5 release.

0.4 (12 May 2015)
-----------------

  - Remove unnecessary `RandomGen g` constraints from `liftRandT`,
    `liftRand`, `evalRandT`, `evalRand`, `runRandT`, `runRand`.

  A major version bump is required by the PVP since the types of all
  the above methods have changed, but this release is again very
  unlikely to break any client code.

0.3.0.2 (30 March 2015)
-----------------------

  - Add `transformers-compat` to allow building with newer `mtl`

0.3.0.1 (24 November 2014)
--------------------------

  - Improve documentation: ranges are exclusive at the upper bound

0.3 (4 September 2014)
----------------------

  - Eta-reduce definition of `Rand`
  - Remove unnecessary `Random a` constraint from types of `liftRand`
    and `liftRandT`.

  Note that a major version bump is required by the PVP since the
  types of `liftRand` and `liftRandT` have changed, but this release
  is highly unlikely to break any client code.

0.2.0.1 (24 August 2014)
------------------------

  - Allow building with both `transformers-0.3` and `0.4`.

0.2 (20 August 2014)
--------------------

  - change `Rand` from a `newtype` to a type synonym
  - `MonadRandom` and `MonadSplit` instances for
      - `ExceptT`
	  - strict variants of `StateT` and `WriterT`
	  - both lazy and strict variants of `RWST`
  - remove unneeded `RandomGen` constraint from `MonadState RandT` instance

0.1.13 (9 February 2014)
------------------------

  - add simple `uniform` function for creating a uniform distribution
    over a list of values

0.1.12 (30 September 2013)
--------------------------

  - add `liftRandT` and `liftRand` functions, for lifting explicit
    generator-passing functions into `RandT` and `Rand`, respectively.

0.1.11 (1 August 2013)
----------------------

  - add `MonadRandom` and `MonadSplit` instances for `IdentityT`
  - derive `MonadReader` and `MonadWriter` instances instead of declaring
    them explicitly (thanks again to James Koppel)

0.1.10 (16 July 2013)
---------------------

  - add `MonadRandom` and `MonadSplit` instances for `ContT`
    (thanks to James Koppel for the patch)

0.1.9 (26 April 2013)
---------------------

  - add `MonadRandom` and `MonadSplit` instances for `MaybeT`
