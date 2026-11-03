proto sub set(|) is pure {*}
multi sub set() {
    # Set up sentinel for all empty immutable Sets
    BEGIN nqp::p6bindattrinvres(
      nqp::create(Set),
      Set,
      '$!elems',
      nqp::create(Rakudo::Internals::IterationSet)
    )
}
multi sub set(*@a --> Set:D) { Set.new(@a) }

proto sub bag(|) is pure {*}
multi sub bag() {
    # Set up sentinel for all empty immutable Bags
    BEGIN nqp::p6bindattrinvres(
      nqp::create(Bag),
      Bag,
      '$!elems',
      nqp::create(Rakudo::Internals::IterationSet)
    )
}
multi sub bag(*@a --> Bag:D) { Bag.new(@a) }

proto sub mix(|) is pure {*}
multi sub mix() {
    # Set up sentinel for all empty immutable Mixes
    BEGIN nqp::p6bindattrinvres(
      nqp::create(Mix),
      Mix,
      '$!elems',
      nqp::create(Rakudo::Internals::IterationSet)
    )
}
multi sub mix(*@a --> Mix:D) { Mix.new(@a) }

# vim: expandtab shiftwidth=4
