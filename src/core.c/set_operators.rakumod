proto sub set(|) is pure {*}
multi sub set() { BEGIN Set.SETUP }
multi sub set(*@a --> Set:D) { Set.new(@a) }

proto sub bag(|) is pure {*}
multi sub bag() { BEGIN Bag.SETUP }
multi sub bag(*@a --> Bag:D) { Bag.new(@a) }

proto sub mix(|) is pure {*}
multi sub mix() { BEGIN Mix.SETUP }
multi sub mix(*@a --> Mix:D) { Mix.new(@a) }

# vim: expandtab shiftwidth=4
