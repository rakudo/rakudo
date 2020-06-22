role Perl6::Metamodel::MultipleInheritance {
    # Array of parents.
    has @!parents;

    # Are any of the parents hidden?
    has @!hides;
    has %!hides_ids;

    # Is this class hidden?
    has $!hidden;

    # Classes to exclude from the parents list in introspection by default.
    my @excluded;
    method exclude_parent($parent) {
        @excluded.push($parent);
    }

    method !rebuild_hides_ids() {
        %!hides_ids := nqp::hash();
        for @!hides {
            nqp::scwbdisable();
            %!hides_ids{~nqp::objectid(nqp::decont($_))} := 1;
            nqp::scwbenable();
        }
    }

    # Adds a parent.
    method add_parent($obj, $parent, :$hides) {
        if self.is_composed($obj) {
            nqp::die("Parents cannot be added to class '" ~ self.name($obj) ~ "'after it has been composed");
        }
        if nqp::decont($parent) =:= nqp::decont($obj) {
            nqp::die("Class " ~ self.name($obj) ~ " cannot inherit from itself");
        }
        my $parent_how := $parent.HOW;
        if nqp::can($parent_how, 'repr_composed') && !$parent_how.repr_composed($parent) {
            my %ex := nqp::gethllsym('Raku', 'P6EX');
            if !nqp::isnull(%ex) && nqp::existskey(%ex, 'X::Inheritance::NotComposed') {
                %ex{'X::Inheritance::NotComposed'}(self.name($obj), $parent_how.name($parent))
            }
            nqp::die("Class " ~ self.name($obj) ~ " cannot inherit from "
                ~ $parent_how.name($parent) ~ " because the parent is not composed yet");
        }
        for @!parents {
            if nqp::decont($_) =:= nqp::decont($parent) {
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
                my @recursive_parents := $_.HOW.parents($_, :tree(1));
                @pt.push(@recursive_parents) if @recursive_parents;
                @result.push(nqp::hllizefor(@pt, 'Raku').Array);
            }
            @result := @result[0] if nqp::elems(@result) == 1;
            return nqp::hllizefor(@result, 'Raku');
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

    method hides_parent($obj, $parent) {
        self.'!rebuild_hides_ids'() if nqp::elems(%!hides_ids) < nqp::elems(@!hides);
        %!hides_ids{~nqp::objectid(nqp::decont($parent))} || 0;
    }

    method hidden($obj) {
        $!hidden ?? 1 !! 0
    }

    method set_hidden($obj) {
        $!hidden := 1;
    }
}

# vim: expandtab sw=4
