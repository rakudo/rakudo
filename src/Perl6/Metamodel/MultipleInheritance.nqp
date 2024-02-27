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
    method add_parent($target, $parent, :$hides) {
        if self.is_composed($target) {
            nqp::die("Parents cannot be added to class '" ~ self.name($target) ~ "'after it has been composed");
        }
        if nqp::decont($parent) =:= nqp::decont($target) {
            nqp::die("Class " ~ self.name($target) ~ " cannot inherit from itself");
        }
        my $parent_how := $parent.HOW;
        if nqp::can($parent_how, 'repr_composed') && !$parent_how.repr_composed($parent) {
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::Inheritance::NotComposed',
                "Class " ~ self.name($target) ~ " cannot inherit from "
                    ~ $parent_how.name($parent) ~ " because the parent is not composed yet",
                :child-name(nqp::hllizefor(self.name($target), 'Raku')),
                :parent-name(nqp::hllizefor($parent_how.name($parent), 'Raku'))
            );
        }
        for @!parents {
            if nqp::decont($_) =:= nqp::decont($parent) {
                nqp::die("Package '" ~ self.name($target) ~
                    "' already has parent '" ~
                    $parent.HOW.name($parent) ~ "'");
            }
        }
        # With a new parent full method list would have to be refreshed.
        if nqp::istype(self, Perl6::Metamodel::MROBasedMethodDispatch) {
            self.invalidate_method_caches($target);
        }
        nqp::push(@!hides,   $parent) if $hides;
        nqp::push(@!parents, $parent);
    }

    # Introspects the parents.
    method parents($target, :$local, :$tree, :$excl, :$all) {
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
            my @mro := self.mro($target);
            my @parents;

            my int $m := nqp::elems(@mro);
            my int $i := 1;  # intentionally skip first
            while $i < +@mro {
                my int $exclude;
                unless $all {
                    for @excluded {
                        $exclude := 1 if @mro[$i] =:= $_;
                    }
                }
                @parents.push(@mro[$i]) unless $exclude;
                ++$i;
            }
            @parents
        }
    }

    method hides($XXX?) { @!hides }

    method hides_parent($XXX, $parent) {
        self.'!rebuild_hides_ids'() if nqp::elems(%!hides_ids) < nqp::elems(@!hides);
        %!hides_ids{~nqp::objectid(nqp::decont($parent))} || 0;
    }

    method hidden($XXX?) { $!hidden ?? 1 !! 0 }

    method set_hidden($XXX?) { $!hidden := 1 }
}

# vim: expandtab sw=4
