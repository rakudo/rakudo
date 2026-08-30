# A SlippyIterator is one that comes with some infrastructure for handling
# flattening a received Slip into its own stream of values.  Please note
# that the $!slipper attribute *must* be set to nqp::null upon object creation.
my role Rakudo::SlippyIterator does Iterator is implementation-detail {
    has Mu $!slipper;  # iterator of the Slip we're iterating, null if none

    # Every multi here takes the Slip raw and decontainerizes it in the
    # body.  Bound to a `$` parameter, a Slip that arrived as a map or
    # grep block's result reaches slip-all inside a container, and the
    # nqp::eqaddr that recognizes Empty compares that container instead.

    proto method start-slip(|) {*}
    multi method start-slip(Slip:U \slip) {
        nqp::decont(slip)
    }
    multi method start-slip(Slip:D \slip) {
        my $slip := nqp::decont(slip);
        nqp::if(
          nqp::eqaddr($slip,Empty),
          IterationEnd,                  # we know there's nothing
          nqp::stmts(
            nqp::if(
              nqp::eqaddr(
                (my $result := ($!slipper := $slip.iterator).pull-one),
                IterationEnd
              ),
              ($!slipper := nqp::null)   # we've determined there's nothing
            ),
            $result
          )
        )
    }

    method slip-one() {
        nqp::if(
          nqp::eqaddr((my $result := $!slipper.pull-one),IterationEnd),
          ($!slipper := nqp::null)
        );
        $result
    }

    # Helper method for pushing the rest of the slipper into the target
    method push-rest(\target --> Nil) {
        $!slipper.push-all(target);
        $!slipper := nqp::null;
    }

    # Helper method for sinking the rest of the slipper
    method sink-rest( --> Nil) {
        $!slipper.sink-all;
        $!slipper := nqp::null;
    }

    # Helper method to handle control exception payloads.  Returns
    # IterationEnd if there was no payload, otherwise the value that
    # was obtained.  Handles Slips.
    method control-payload() is raw {
        nqp::if(
          nqp::isnull(my $value := nqp::getpayload(nqp::exception)),
          IterationEnd,
          nqp::if(
            nqp::istype($value,Slip),
            self.start-slip($value),
            $value
          )
        )
    }

    # Process the payload of a control exception and push it to the
    # target while following Slip semantics.
    method push-control-payload(\target --> Nil) {
        nqp::unless(
          nqp::isnull(my $value := nqp::getpayload(nqp::exception)),
          nqp::if(
            nqp::istype($value,Slip),
            self.slip-all($value,target),
            target.push($value)
          )
        )
    }

    proto method slip-all(|) {*}
    multi method slip-all(Slip:U \slip, \target) {
        target.push(nqp::decont(slip))
    }
    multi method slip-all(Slip:D \slip, \target) {
        my $slip := nqp::decont(slip);
        nqp::unless(
          nqp::eqaddr($slip,Empty),
          $slip.iterator.push-all(target)
        )
    }
    method is-deterministic(--> False) { }
}

# vim: expandtab shiftwidth=4
