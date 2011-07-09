role Perl6::Metamodel::MultipleInheritance {
    # Array of parents.
    has @!parents;

    # Adds a parent.
    method add_parent($obj, $parent) {
        if self.is_composed($obj) {
            pir::die("Parents cannot be added to a class after it has been composed");
        }
        if $parent =:= $obj {
            pir::die("Class " ~ self.name($obj) ~ " cannot inherit from itself");
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
    method parents($obj, :$local, :$tree) {
        if $local {
            @!parents
        }
        elsif $tree {
            my @result;
            for @!parents {
                my @pt := [$_];
                @pt.push($_.HOW.parents($_, :tree(1)));
                @result.push(pir::perl6ize_type__PP(@pt).Array.item);
            }
            return pir::perl6ize_type__PP(@result);
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
