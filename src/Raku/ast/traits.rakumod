# Done by everything that can have traits applied to it.
class RakuAST::TraitTarget {
    has Mu $!traits;
    has List $!sorries;
    has List $!worries;

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
        my $traits := $!traits;
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

    method traits-include-is-generic() {
        my int $has-is-generic := 0;
        for self.IMPL-UNWRAP-LIST(self.traits) -> $trait {
            last if $has-is-generic := nqp::istype($trait,RakuAST::Trait::Is)
                                    && $trait.type
                                    && $trait.type.meta-object.HOW.archetypes.generic
        }
        $has-is-generic
    }

    method add-trait-sorries() {
        if $!sorries {
            self.add-sorry($_) for $!sorries;
        }
        if $!worries {
            self.add-worry($_) for $!worries;
        }
    }

    # Apply all traits (and already applied will not be applied again).
    method apply-traits(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context, RakuAST::TraitTarget $target, *%named) {
        if $!traits {
            my constant is-traits-to-warn-on-duplicate := nqp::hash(
                'tighter',  1,  'looser', 1,  'equiv', 1,  'rw',   1,  'default', 1,
                'readonly', 1,  'raw',    1,  'assoc', 1,  'pure', 1,  'export',  1,
                'item', 1
            );
            my %seen;
            for $!traits {
                $_.apply($resolver, $context, $target, |%named) unless $_.applied;
                CATCH {
                    nqp::bindattr(self, RakuAST::TraitTarget, '$!sorries', []) unless nqp::isconcrete($!sorries);
                    my $ex := nqp::getpayload($_);
                    if $ex {
                        my $XUndeclaredSymbols := $resolver.resolve-name-constant-in-setting(
                            RakuAST::Name.from-identifier-parts('X', 'Inheritance', 'UnknownParent'));
                        if nqp::istype($ex, $XUndeclaredSymbols.compile-time-value) {
                            for $resolver.suggest-typename($ex.parent) {
                                $ex.suggestions.push($_)
                            }
                        }
                    }
                    else {
                        $ex := $resolver.build-exception: 'X::AdHoc', :payload(nqp::getmessage($_))
                            unless nqp::isconcrete($ex);
                    }
                    nqp::push($!sorries, $ex);
                }
                CONTROL {
                    if nqp::getextype($_) == nqp::const::CONTROL_WARN {
                        nqp::bindattr(self, RakuAST::TraitTarget, '$!worries', []) unless nqp::isconcrete($!worries);
                        my $ex := nqp::getpayload($_);
                        $ex := $resolver.build-exception: 'X::AdHoc', :payload(nqp::getmessage($_))
                            unless nqp::isconcrete($ex);
                        nqp::push($!worries, $ex);
                        nqp::resume($_);
                    }
                    nqp::rethrow($_);
                }
                my $name := (try $_.name.canonicalize) // '';
                if is-traits-to-warn-on-duplicate{$name} && %seen{$name}++ {
                    $resolver.add-worry: $resolver.build-exception: 'X::AdHoc', :payload("Duplicate '" ~ $_.IMPL-TRAIT-NAME() ~ " $name' trait");
                }
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
class RakuAST::Trait
  is RakuAST::ImplicitLookups
{
    has int $!applied;

    method IMPL-TRAIT-NAME() {
        nqp::die(self.HOW.name(self) ~ ' does not implement IMPL-TRAIT-NAME')
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Var::Lexical::Constant.new('&trait_mod:<' ~ self.IMPL-TRAIT-NAME() ~ '>')
        ]
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
    method apply(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context, RakuAST::TraitTarget $target, *%named) {
        unless self.applied {
            self.to-begin-time($resolver, $context);
            my $decl-target := RakuAST::Declaration::ResolvedConstant.new:
                compile-time-value => $target.compile-time-value;
            my $args := self.IMPL-TRAIT-ARGS($resolver, $decl-target);
            for %named {
                nqp::push(
                    self.IMPL-UNWRAP-LIST($args.args),
                    RakuAST::ColonPair::Value.new(:key(nqp::iterkey_s($_)), :value(nqp::iterval($_)))
                );
            }
            $args.IMPL-BEGIN($resolver, $context);
            $target.IMPL-BEGIN-TIME-CALL(
              self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0],
              $args,
              $resolver,
              $context
            );
            self.mark-applied;
        }
    }
}

# The is trait.
class RakuAST::Trait::Is
  is RakuAST::Trait
  is RakuAST::BeginTime
{
    has RakuAST::Name $.name;
    has RakuAST::Circumfix $.argument;
    has RakuAST::Type $.type;

    method new(RakuAST::Name :$name!, RakuAST::Circumfix :$argument) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Trait::Is, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Trait::Is, '$!argument',
            $argument // RakuAST::Circumfix);
        $obj
    }

    method new-from-type(RakuAST::Type :$type!, RakuAST::Circumfix :$argument) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Trait::Is, '$!type', $type);
        nqp::bindattr($obj, RakuAST::Trait::Is, '$!argument',
            $argument // RakuAST::Circumfix);
        $obj
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # See if the name resolves as a type and commit to that.
        unless $!type {
            my $resolution := $resolver.resolve-name-constant($!name);
            if nqp::istype($resolution, RakuAST::CompileTimeValue) &&
                    !nqp::isconcrete($resolution.compile-time-value) {
                my $type := RakuAST::Type::Simple.new($!name);
                $type.set-resolution($resolution);
                nqp::bindattr(self, RakuAST::Trait::Is, '$!type', $type);
            }
        }
        Nil
    }

    method IMPL-TRAIT-NAME() { 'is' }

    method IMPL-TRAIT-ARGS(RakuAST::Resolver $resolver, RakuAST::Node $target) {
        my @args := [$target];
        if $!type {
            @args.push($!type);
            @args.push($!argument) if $!argument;
        }
        else {
            my $key := $!name.canonicalize;
            @args.push(
                $!argument
                ?? RakuAST::ColonPair::Value.new(:$key, :value($!argument))
                !! RakuAST::ColonPair::True.new($key)
            );
        }
        RakuAST::ArgList.new(|@args)
    }

    method visit-children(Code $visitor) {
        $visitor($!name) if $!name;
        $visitor($!argument) if $!argument;
        $visitor($!type) if $!type;
    }
}

class RakuAST::Trait::Type
  is RakuAST::Trait
{
    has RakuAST::Type $.type;

    method new(RakuAST::Type $type) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Trait::Type, '$!type', $type);
        $obj
    }

    method IMPL-TRAIT-ARGS(RakuAST::Resolver $resolver, RakuAST::Node $target) {
        RakuAST::ArgList.new($target, $!type)
    }

    method visit-children(Code $visitor) {
        $visitor($!type);
    }
}

# The hides trait.
class RakuAST::Trait::Hides
  is RakuAST::Trait::Type
{
    method IMPL-TRAIT-NAME() { 'hides' }
}

# The does trait.
class RakuAST::Trait::Does
  is RakuAST::Trait::Type
{
    method IMPL-TRAIT-NAME() { 'does' }
}

# The of trait.
class RakuAST::Trait::Of
  is RakuAST::Trait::Type
{
    method IMPL-TRAIT-NAME() { 'of' }
}

# The returns trait.
class RakuAST::Trait::Returns
  is RakuAST::Trait::Type
{
    method IMPL-TRAIT-NAME() { 'returns' }
}

# The will build trait on attributes. For internal usage only.
class RakuAST::Trait::WillBuild
  is RakuAST::Trait
{
    has RakuAST::Expression $.expr;

    method new(RakuAST::Expression $expr) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Trait::WillBuild, '$!expr', $expr);
        $obj
    }

    method IMPL-TRAIT-NAME() { 'will' }

    method IMPL-TRAIT-ARGS(RakuAST::Resolver $resolver, RakuAST::Node $target) {
        RakuAST::ArgList.new($target, RakuAST::ColonPair::Value.new(:key('build'), :value($!expr)))
    }

    method visit-children(Code $visitor) {
        $visitor($!expr);
    }
}

# The will trait.
class RakuAST::Trait::Will
  is RakuAST::Trait
{
    has str $.phase;
    has RakuAST::Block $.block;

    method new(str :$phase, RakuAST::Block :$block) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Trait::Will, '$!phase', $phase);
        nqp::bindattr($obj, RakuAST::Trait::Will, '$!block', $block);
        $obj
    }

    method IMPL-TRAIT-NAME() { 'will' }

    method IMPL-TRAIT-ARGS(RakuAST::Resolver $resolver, RakuAST::Node $target) {
        RakuAST::ArgList.new($target, $!block, RakuAST::ColonPair::True.new($!phase))
    }

    method visit-children(Code $visitor) {
        $visitor($!block);
    }
}

class RakuAST::Trait::Handles
  is RakuAST::Trait
{
    has RakuAST::Term $.term;

    method new(RakuAST::Term $term) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Trait::Handles, '$!term', $term);
        $obj
    }

    method IMPL-TRAIT-NAME() { 'handles' }

    method IMPL-TRAIT-ARGS(RakuAST::Resolver $resolver, RakuAST::Node $target) {
        my $block := RakuAST::Block.new:
                        body => RakuAST::Blockoid.new:
                            RakuAST::StatementList.new:
                                RakuAST::Statement::Expression.new:
                                    expression => $!term;
        RakuAST::ArgList.new($target, $block);
    }

    method visit-children(Code $visitor) {
        $visitor($!term);
    }
}

class RakuAST::Trait::Trusts
  is RakuAST::Trait
{
    has RakuAST::Type $.type;

    method new(RakuAST::Type :$type!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Trait::Trusts, '$!type', $type);
        $obj
    }

    method IMPL-TRAIT-NAME() { 'trusts' }

    method IMPL-TRAIT-ARGS(RakuAST::Resolver $resolver, RakuAST::Node $target) {
        RakuAST::ArgList.new($target, $!type)
    }

    method visit-children(Code $visitor) {
        $visitor($!type) if $!type;
    }
}
