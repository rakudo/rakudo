#- Metamodel::MultipleInheritance ----------------------------------------------
# The logic to handle classes that inherit from more than one class
role Perl6::Metamodel::MultipleInheritance {
    # Array of parents.
    has @!parents;

    # Are any of the parents hidden?
    has @!hides;
    has %!hides_ids;

    # Is this class hidden?
    has int $!hidden;

    # Classes to exclude from the parents list in introspection by default.
    my @excluded;
    method exclude_parent($parent) {
        nqp::push(@excluded, nqp::decont($parent))
    }

    method !rebuild_hides_ids() {
        self.protect({
            my @hides := @!hides;

            my %hides_ids;
            my int $m := nqp::elems(@hides);
            my int $i;

            nqp::scwbdisable;
            while $i < $m {
                nqp::bindkey(
                  %hides_ids,
                  nqp::objectid(nqp::decont(nqp::atpos(@hides, $i))),
                  1
                );
                ++$i
            }
            nqp::scwbenable;

            %!hides_ids := %hides_ids;
        });
    }

    # Adds a parent.
    method add_parent($target, $parent, :$hides) {
        $target            := nqp::decont($target);
        my str $child-name := self.name($target);

        $parent             := nqp::decont($parent);
        my $parentHOW       := $parent.HOW;
        my str $parent-name := $parentHOW.name($parent);

        nqp::die("Parents cannot be added to class '"
          ~ $child-name
          ~ "'after it has been composed"
        ) if self.is_composed($target);

        nqp::die("Class $child-name cannot inherit from itself")
          if nqp::eqaddr($parent, $target);

        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Inheritance::NotComposed',
          "Class $child-name cannot inherit from $parent-name because the parent is not composed yet",
          :$child-name,
          :$parent-name
        ) if nqp::can($parentHOW, 'repr_composed')
          && !$parentHOW.repr_composed($parent);

        self.protect({
            my @parents := nqp::clone(@!parents);
            my int $m := nqp::elems(@parents);
            my int $i;
            while $i < $m {
                nqp::eqaddr(nqp::atpos(@parents, $i), $parent)
                  ?? nqp::die("Package '"
                       ~ $child-name
                       ~ "' already has parent '"
                       ~ $parent-name
                       ~ "'"
                     )
                  !! ++$i
            }

            # Creating full method list needs a computed MRO.
            self.invalidate_mro_cache($target)
              if nqp::istype(self, Perl6::Metamodel::C3MRO);
            # With a new parent full method list would have to be refreshed.
            self.invalidate_method_caches($target)
              if nqp::istype(self, Perl6::Metamodel::MROBasedMethodDispatch);

            if $hides {
                my @hides := nqp::clone(@!hides);
                nqp::push(@hides, $parent);
                @!hides := @hides;
            }

            nqp::push(@parents, $parent);
            @!parents := @parents;
        });
    }

    # Introspects the parents.
    method parents($target, :$local, :$tree, :$excl, :$all) {
        if $local {
            nqp::clone(@!parents)
        }
        elsif $tree {
            my @parents := @!parents;
            my @result;

            my int $m := nqp::elems(@parents);
            my int $i;
            while $i < $m {
                my $parent := nqp::atpos(@parents, $i);
                my @rp     := $parent.HOW.parents($parent, :tree);
                nqp::push(
                  @result,
                  nqp::hllizefor(
                    @rp.elems  # because @rp is a HLL Array
                      ?? nqp::list($parent, @rp)
                      !! nqp::list($parent),
                    'Raku'
                  ).Array    # XXX why the Array???
                );
                ++$i;
            }

            nqp::hllizefor(
              nqp::elems(@result) == 1
                ?? nqp::atpos(@result, 0)
                !! @result ,
              'Raku'
            )
        }

        # We want it all
        elsif $all {
            my @parents := nqp::clone(self.mro($target));
            nqp::shift(@parents);  # lose ourselves
            @parents
        }

        # All, but with excluded parents excluded
        else {

            # Helper sub to check whether a parent is considered to be excluded
            sub is_excluded($parent) {
                my int $m := nqp::elems(@excluded);
                my int $i;
                while $i < $m {
                    nqp::eqaddr(nqp::atpos(@excluded, $i), $parent)
                      ?? (return 1)
                      !! ++$i;
                }
                0
            }

            # All parents is MRO minus the first thing (which is us).
            my @mro := self.mro($target);
            my @parents;

            my int $m := nqp::elems(@mro);
            my int $i := 1;  # intentionally skip first
            while $i < $m {
                my $parent := nqp::atpos(@mro, $i);
                nqp::push(@parents, $parent) unless is_excluded($parent);
                ++$i;
            }
            @parents
        }
    }

    method hides($XXX?) { @!hides }

    method hides_parent($XXX, $parent) {
        self.'!rebuild_hides_ids'()
          if nqp::elems(%!hides_ids) < nqp::elems(@!hides);
        nqp::ifnull(
          nqp::atkey(%!hides_ids, nqp::objectid(nqp::decont($parent))),
          0
        )
    }

    method hidden($XXX?) { $!hidden }

    method set_hidden($XXX?) { $!hidden := 1 }
}

# vim: expandtab sw=4
