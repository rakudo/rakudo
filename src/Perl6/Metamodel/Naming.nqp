role Perl6::Metamodel::Naming {
    has $!name;
    has $!shortname;

    method name($obj) {
        $!name // ($!name := '')
    }

    method set_name($obj, $name) {
        $!name      := $name;
        $!shortname := NQPMu; # Gets set once it's needed.
        nqp::setdebugtypename($obj, $name);
    }

    method shortname($obj) {
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

    method set_shortname($obj, $shortname) {
        $!shortname := $shortname;
    }
}

# vim: expandtab sw=4
