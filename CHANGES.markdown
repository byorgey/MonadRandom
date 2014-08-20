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

  - add simple 'uniform' function for creating a uniform distribution
    over a list of values

0.1.12 (30 September 2013)
--------------------------

  - add liftRandT and liftRand functions, for lifting explicit
    generator-passing functions into RandT and Rand, respectively.

0.1.11 (1 August 2013)
----------------------

  - add MonadRandom and MonadSplit instances for IdentityT
  - derive MonadReader and MonadWriter instances instead of declaring
    them explicitly (thanks again to James Koppel)

0.1.10 (16 July 2013)
---------------------

  - add MonadRandom and MonadSplit instances for ContT
    (thanks to James Koppel for the patch)

0.1.9 (26 April 2013)
---------------------

  - add MonadRandom and MonadSplit instances for MaybeT
