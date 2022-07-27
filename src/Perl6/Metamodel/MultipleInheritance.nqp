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
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::Inheritance::NotComposed',
                "Class " ~ self.name($obj) ~ " cannot inherit from "
                    ~ $parent_how.name($parent) ~ " because the parent is not composed yet",
                :child-name(nqp::hllizefor(self.name($obj), 'Raku')),
                :parent-name(nqp::hllizefor($parent_how.name($parent), 'Raku'))
            );
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

    # If truthy, seals inheritance, at which point the MRO of a nascent type
    # object should exist. This is optional, thus the ??? stub.
    method is_composed($obj) {
        0
    }

    # Introspects the parents.
    method parents($obj, :$local = 0, :$tree = 0, :$all = 0, :$excl) {
        $local
            ?? @!parents
            !! $tree
                ?? self.parents-tree($obj, @!parents)
                !! self.is_composed($obj)
                    ?? self.parents-off-mro($obj, self.mro($obj), :$all)
                    !! self.parents-ordered($obj, @!parents, :$all)
    }

    my &PARENTS-TREE := nqp::getstaticcode(
        anon sub PARENTS-TREE(@self, $obj) {
            (my @parents := $obj.HOW.parents($obj, :tree))
                ?? @self.accept(nqp::list($obj, @parents))
                !! @self.accept(nqp::list($obj))
        });

    # Produces the hierarchy of a type.
    method parents-tree($obj, @parents) {
        @parents := $monic_machine.new.veneer(@parents);
        my @tree := @parents.banish(&PARENTS-TREE, nqp::list());
        nqp::elems(@tree) == 1 ?? @tree[0] !! @tree
    }

    # Produces a cached ordering for the parents metamethod.
    method parents-off-mro($obj, @mro, :$all = 0) {
        nqp::shift(my @parents := nqp::clone(@mro));
        $all
            ?? @parents
            !! $monic_machine.new.veneer(@parents).efface(@excluded, nqp::list())
    }

    my &PARENTS-ALL := nqp::getstaticcode(
        anon sub PARENTS-ALL(@self, $obj) {
            @self.accept(nqp::clone($obj.HOW.mro($obj)))
        });

    # Produces an ordering for the parents metamethod. By default, this is a C3
    # linearization, but may be overridden for a different ordering.
    method parents-ordered($obj, @parents, :$all = 0) {
        @parents := $monic_machine.new.veneer(@parents);
        $all
            ?? @parents.summon(&PARENTS-ALL).beckon(nqp::list())
            !! @parents.summon(&PARENTS-ALL).beckon(@parents.new).efface(@excluded, nqp::list())
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
