role Perl6::Metamodel::MultipleInheritance {
    # Array of parents.
    has @!parents;

    # Adds a parent.
    method add_parent($obj, $parent) {
        if self.composed($obj) {
            pir::die("Parents cannot be added to a class after it has been composed");
        }
        for @!parents {
            if $_ =:= $parent {
                pir::die("Package '" ~ self.name($obj) ~
                    "' already has parent '" ~
                    $parent.HOW.name($parent) ~ "'");
            }
        }
        @!parents[+@!parents] := $parent;
    }

    # Introspects the parents.
    method parents($obj, :$local) {
        if $local {
            @!parents
        }
        else {
            # All parents is MRO minus the first thing (which is us).
            my @mro := self.mro($obj);
            my @parents;
            my $i := 1;
            while $i < +@mro {
                @parents.push(@mro[$i]);
                $i := $i + 1;
            }
            @parents
        }
    }
}
