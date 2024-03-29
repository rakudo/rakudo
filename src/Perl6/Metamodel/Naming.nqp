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
}

# vim: expandtab sw=4
