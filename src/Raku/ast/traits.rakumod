# Done by everything that can have traits applied to it.
class RakuAST::TraitTarget {
    has Mu $!traits;

    # Set the list of traits on this declaration.
    method set-traits(List $traits) {
        my @traits;
        if $traits {
            for self.IMPL-UNWRAP-LIST($traits) {
                unless nqp::istype($_, RakuAST::Trait) {
                    nqp::die('The traits list can only contain RakuAST::Trait objects');
                }
                nqp::push(@traits, $_);
            }
        }
        nqp::bindattr(self, RakuAST::TraitTarget, '$!traits', @traits);
        Nil
    }

    # Add a trait to this declaration.
    method add-trait(RakuAST::Trait $trait) {
        my $traits;
        unless nqp::islist($traits) {
            $traits := [];
            nqp::bindattr(self, RakuAST::TraitTarget, '$!traits', $traits);
        }
        nqp::push($traits, $trait);
        Nil
    }

    # Get the list of traits on this declaration.
    method traits() {
        my $traits := $!traits;
        self.IMPL-WRAP-LIST(nqp::islist($traits) ?? $traits !! [])
    }

    # Apply all traits (and already applied will not be applied again).
    method apply-traits(RakuAST::Resolver $resolver, RakuAST::TraitTarget $target) {
        if $!traits {
            for $!traits {
                $_.apply($resolver, $target) unless $_.applied;
            }
        }
        Nil
    }

    # Apply the visitor to each trait on this declaration.
    method visit-traits(Code $visitor) {
        if $!traits {
            for $!traits {
                $visitor($_);
            }
        }
    }
}

# The base of all traits.
class RakuAST::Trait is RakuAST::ImplicitLookups {
    has int $!applied;

    method IMPL-TRAIT-NAME() {
        nqp::die(self.HOW.name(self) ~ ' does not implement IMPL-TRAIT-NAME')
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&trait_mod:<' ~ self.IMPL-TRAIT-NAME() ~ '>')
        ])
    }

    # Checks if this trait has been applied already.
    method applied() {
        $!applied ?? True !! False
    }

    # Marks the trait as having been applied. Typically used when the trait is
    # specially handled by a construct rather than actually being dispatched
    # to a trait handler (for example, `is repr` on packages).
    method mark-applied() {
        nqp::bindattr_i(self, RakuAST::Trait, '$!applied', 1);
        Nil
    }

    # Apply the trait to the specified target. Checks if it has been applied,
    # and then applies it.
    method apply(RakuAST::Resolver $resolver, RakuAST::TraitTarget $target) {
        unless self.applied {
            self.IMPL-CHECK($resolver, False);
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
            my $args := self.IMPL-TRAIT-ARGS($resolver, $target);
            $target.IMPL-BEGIN-TIME-CALL(@lookups[0], $args, $resolver);
            self.mark-applied;
        }
    }
}

# The hides trait.
class RakuAST::Trait::Hides is RakuAST::Trait {
    has RakuAST::Type $.type;

    method new(RakuAST::Type $type) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Trait::Hides, '$!type', $type);
        $obj
    }

    method IMPL-TRAIT-NAME() { 'hides' }

    method IMPL-TRAIT-ARGS(RakuAST::Resolver $resolver, RakuAST::TraitTarget $target) {
        RakuAST::ArgList.new($target, $!type)
    }

    method visit-children(Code $visitor) {
        $visitor($!type);
    }
}

# The does trait.
class RakuAST::Trait::Does is RakuAST::Trait {
    has RakuAST::Type $.type;

    method new(RakuAST::Type $type) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Trait::Does, '$!type', $type);
        $obj
    }

    method IMPL-TRAIT-NAME() { 'does' }

    method IMPL-TRAIT-ARGS(RakuAST::Resolver $resolver, RakuAST::TraitTarget $target) {
        RakuAST::ArgList.new($target, $!type)
    }

    method visit-children(Code $visitor) {
        $visitor($!type);
    }
}

# The of trait.
class RakuAST::Trait::Of is RakuAST::Trait {
    has RakuAST::Type $.type;

    method new(RakuAST::Type $type) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Trait::Of, '$!type', $type);
        $obj
    }

    method IMPL-TRAIT-NAME() { 'of' }

    method IMPL-TRAIT-ARGS(RakuAST::Resolver $resolver, RakuAST::TraitTarget $target) {
        RakuAST::ArgList.new($target, $!type)
    }

    method visit-children(Code $visitor) {
        $visitor($!type);
    }
}

# The returns trait.
class RakuAST::Trait::Returns is RakuAST::Trait {
    has RakuAST::Type $.type;

    method new(RakuAST::Type $type) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Trait::Of, '$!type', $type);
        $obj
    }

    method IMPL-TRAIT-NAME() { 'returns' }

    method IMPL-TRAIT-ARGS(RakuAST::Resolver $resolver, RakuAST::TraitTarget $target) {
        RakuAST::ArgList.new($target, $!type)
    }

    method visit-children(Code $visitor) {
        $visitor($!type);
    }
}
