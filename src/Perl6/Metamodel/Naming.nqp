role Perl6::Metamodel::Naming {
    has $!name;
    has $!shortname;

    method name($XXX?) { $!name // ($!name := '') }

    method set_name($target, $name) {
        $!name      := $name;
        $!shortname := NQPMu; # Gets set once it's needed.
        nqp::setdebugtypename($target, $name);
    }

    method shortname($XXX?) {
        sub to_shortname($name) {
            return '' unless $name;

            my $shortname := $name;
            while (my int $colon := nqp::rindex($shortname, '::')) >= 0 {
                my int $paren := nqp::rindex($shortname, '[', $colon - 1);
                my int $comma := nqp::rindex($shortname, ',', $colon - 1);
                my int $chop-start :=
                    ($paren < 0 && $comma < 0)
                    ?? 0
                    !! ($paren >= 0 && $paren < $comma)
                        ?? $comma + 1
                        !! $paren + 1;
                $shortname := nqp::concat(
                    nqp::substr($shortname, 0, $chop-start),
                    nqp::substr($shortname, $colon + 2)
                );
            }
            $shortname
        }

        $!shortname // ($!shortname := to_shortname($!name))
    }

    method set_shortname($XXX, $shortname) {
        $!shortname := $shortname;
    }

#-------------------------------------------------------------------------------
# Note that this locking logic has nothing to do with naming.  But it was
# the only way to have "protect" functionality on all fooHOW classes,
# including ones in the ecosystem, most notably Inline::Perl5.

    has $!locking;

    method TWEAK(*%_) { $!locking := NQPLock.new }

    method protect(&code) { $!locking.protect(&code) }
#-------------------------------------------------------------------------------
# Note that this helper logic has nothing to do with naming.  But it was
# deemed to be a good place to allow all fooHOW classes to have them.

    # Helper method to return 1 if any of the types in the given list of types
    # matches the checkee, else 0
    method list_istype_checkee(@types, $checkee) {
        my int $m   := nqp::elems(@types);
        my int $i;
        while $i < $m {
            nqp::istype(nqp::atpos(@types, $i), $checkee)
              ?? (return 1)
              !! ++$i;
        }
        0
    }

    # Helper method to return 1 if the checkee matches the type of any of
    # the types in the given list of types, else 0
    method checkee_istype_list($checkee, @types) {
        my int $m   := nqp::elems(@types);
        my int $i;
        while $i < $m {
            nqp::istype($checkee, nqp::atpos(@types, $i))
              ?? (return 1)
              !! ++$i;
        }
        0
    }

    # Helper method to return 1 if the checkee is the same as the type of any
    # of the types in the given list of types, else 0
    method checkee_eqaddr_list($checkee, @types) {
        my int $m   := nqp::elems(@types);
        my int $i;
        while $i < $m {
            nqp::eqaddr($checkee, nqp::decont(nqp::atpos(@types, $i)))
              ?? (return 1)
              !! ++$i;
        }
        0
    }
}

# vim: expandtab sw=4
