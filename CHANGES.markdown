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
