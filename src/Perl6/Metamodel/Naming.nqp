role Perl6::Metamodel::Naming {
    has $!name;
    has $!shortname;
    method set_name($obj, $name) {
        $!name := $name;
        nqp::setdebugtypename($obj, $name);
        if $name {
            while (my int $colon := nqp::rindex($name, '::')) != -1 {
                my int $paren := nqp::rindex($name, '[', $colon - 1);
                my int $comma := nqp::rindex($name, ',', $colon - 1);
                my int $chop-start :=
                    ($paren < 0 && $comma < 0)
                    ?? 0
                    !! ($paren != -1 && $paren < $comma)
                        ?? $comma + 1
                        !! $paren + 1;
                $name := nqp::concat(
                    nqp::substr($name, 0, $chop-start),
                    nqp::substr($name, $colon + 2)
                );
            }

            $!shortname := $name;
        }
        else {
            $!shortname := '';
        }
    }
    method set_shortname($obj, $shortname) {
        $!shortname := $shortname;
    }
    method name($obj) {
        $!name
    }
    method shortname($obj) {
        $!shortname
    }
}
