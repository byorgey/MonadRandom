0.5 (26 July 2016)
------------------

  - Refactor to reflect structure of `mtl` and `transformers` libraries.
  - Improve documentation.
  - Add lazy and strict variants of `RandT`.
  - Add `MonadRandom` and `MonadSplit` instances for `ListT`.
  - Add (but do not export) `unRandT` field to `RandT`.
  - Add `MonadCont`, `MonadError` and `MonadRWS` instances for `RandT`.
  - Add signatures for `RandT` operations that require specialized lifting (see "Control.Monad.Signatures").
  - Remove `evalRandIO` operation.
  - Move `fromList` and `uniform` operations to "Control.Monad.Random.Class".
  - `fromList` operation raises error with total weight of elements is zero.

  This patch requires a major version bump, as it may incur breaking changes
  for modules that import `Control.Monad.Random` only. All modules have been
  refactored to better reflect the structure of the `mtl` and `transformers`
  libraries.
  
  Lazy and strict variants of `RandT` are implemented, with lazy as the
  default. Instances of all `mtl` classes are provided for `RandT`. Instances
  of `MonadRandom` and `MonadSplit` are provided for all `transformers` types.

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
