role Perl6::Metamodel::MultipleInheritance {
    # Array of parents.
    has @!parents;
    
    # Are any of the parents hidden?
    has @!hides;
    
    # Is this class hidden?
    has $!hidden;
    
    # Classes to exclude from the parents list in introspection by default.
    my @excluded;
    method exclude_parent($parent) {
        @excluded.push($parent);
    }

    # Adds a parent.
    method add_parent($obj, $parent, :$hides) {
        if self.is_composed($obj) {
            nqp::die("Parents cannot be added to a class after it has been composed");
        }
        if $parent =:= $obj {
            nqp::die("Class " ~ self.name($obj) ~ " cannot inherit from itself");
        }
        for @!parents {
            if $_ =:= $parent {
                nqp::die("Package '" ~ self.name($obj) ~
                    "' already has parent '" ~
                    $parent.HOW.name($parent) ~ "'");
            }
        }
        if $hides {
            @!hides[+@!hides] := $parent;
        }
        @!parents[+@!parents] := $parent;
    }

    # Introspects the parents.
    method parents($obj, :$local, :$tree, :$excl, :$all) {
        if $local {
            @!parents
        }
        elsif $tree {
            my @result;
            for @!parents {
                my @pt := [$_];
                @pt.push($_.HOW.parents($_, :tree(1)));
                @result.push(nqp::hllizefor(@pt, 'perl6').Array.item);
            }
            return nqp::hllizefor(@result, 'perl6');
        }
        else {
            # All parents is MRO minus the first thing (which is us).
            my @mro := self.mro($obj);
            my @parents;
            my $i := 1;
            while $i < +@mro {
                my $exclude := 0;
                unless $all {
                    for @excluded {
                        $exclude := 1 if @mro[$i] =:= $_;
                    }
                }
                @parents.push(@mro[$i]) unless $exclude;
                $i := $i + 1;
            }
            @parents
        }
    }
    
    method hides($obj) {
        @!hides
    }
    
    method hidden($obj) {
        $!hidden ?? 1 !! 0
    }
    
    method set_hidden($obj) {
        $!hidden := 1;
    }
}
