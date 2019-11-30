
# Recompose classes likely to have been affected by classes added for this
# language version.  Please note that the *order* in which the recomposes
# are done, is important to ensure that all classes see the most up-to-date
# version of its method caches.

BEGIN {
    for (
      Map,
      Hash,
      Set,
      SetHash,
      Bag,
      BagHash,
      Mix,
      MixHash,
    ) -> \type {
          type.^compose
    }
}

# vim: ft=perl6 expandtab sw=4
