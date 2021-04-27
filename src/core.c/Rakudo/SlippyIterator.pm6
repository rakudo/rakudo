# A SlippyIterator is one that comes with some infrastructure for handling
# flattening a received Slip into its own stream of values.
my role Rakudo::SlippyIterator does Iterator {
    # Flat set to non-zero if the iterator is currently consuming a Slip.
    has int $!slipping;

    # The current Slip we're iterating.
    has $!slip-iter;

    proto method start-slip(|) {*}
    multi method start-slip(Slip:U $slip) {
        $slip
    }
    multi method start-slip(Slip:D $slip) {
        nqp::if(
          nqp::eqaddr($slip,Empty),
          IterationEnd,                  # we know there's nothing
          nqp::if(
            nqp::eqaddr(
              (my \result := ($!slip-iter := $slip.iterator).pull-one),
              IterationEnd
            ),
            IterationEnd,                # we've determined there's nothing
            nqp::stmts(                  # need to start a Slip
              ($!slipping = 1),
              result
            )
          )
        )
    }

    method slip-one() {
        nqp::if(
          nqp::eqaddr((my \result := $!slip-iter.pull-one),IterationEnd),
          nqp::stmts(
            ($!slipping = 0),
            ($!slip-iter := nqp::null)
          )
        );
        result
    }

    proto method slip-all(|) {*}
    multi method slip-all(Slip:U $slip, \target) {
        target.push($slip)
    }
    multi method slip-all(Slip:D $slip, \target) {
        nqp::unless(
          nqp::eqaddr($slip,Empty),
          $slip.iterator.push-all(target)
        )
    }
    method is-deterministic(--> False) { }
}

# vim: expandtab shiftwidth=4
