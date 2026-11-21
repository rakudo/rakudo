# The commonalities for placeholder parameters.
class RakuAST::VarDeclaration::Placeholder
  is RakuAST::Term
  does RakuAST::VarDeclaration
  does RakuAST::BeginTime
{
    has Bool $!already-declared;
    has RakuAST::Node $!owner;
    has RakuAST::VarDeclaration::Simple $!lowering-declaration;
    has int $!refused;

    method lexical-name() { nqp::die('Missing lexical-name implementation') }

    method declared-name() { self.lexical-name }

    method generate-parameter() {
        nqp::die('Missing generate-parameter implementation')
    }

    method default-scope() { 'my' }

    method allowed-scopes() { ['my'] }

    method generate-lookup() {
        my $lookup := RakuAST::Var::Lexical.new(self.lexical-name);
        $lookup.set-resolution(self);
        $lookup
    }

    method is-simple-lexical-declaration() {
        False
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $owner := $resolver.find-attach-target('block');
        my $method := $resolver.find-attach-target('method');
        # A heredoc body written after the closing brace of a block cannot add
        # a parameter to it. A %_ in a method is the method's own slurpy, so it
        # adds nothing.
        if $resolver.IMPL-HEREDOC-SCOPE-CLOSED($owner)
          && !(self.lexical-name eq '%_' && ($method || $owner.IMPL-IS-IN-METHOD)) {
            nqp::bindattr_i(self, RakuAST::VarDeclaration::Placeholder, '$!refused', 1);
            self.add-sorry: $resolver.build-exception: 'X::Placeholder::Mainline',
              placeholder => self.declared-name;
            return Nil;
        }
        if $owner {
            nqp::bindattr(self, RakuAST::VarDeclaration::Placeholder, '$!owner', $owner);
            $owner.add-placeholder-parameter(self);
            $owner.add-generated-lexical-declaration(self)
                unless $!already-declared || self.lexical-name eq '%_' && $method;
        }
    }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        return True if $!refused;
        my $block := $resolver.find-attach-target('block');
        my $name := self.declared-name;
        my $lexical-name := self.lexical-name;

        if $block {
            my $method := $resolver.find-attach-target('method');
            if nqp::istype($block, RakuAST::Block) && !$block.may-have-signature
                && !($lexical-name eq '%_' && $method)
            {
                self.add-sorry:
                    $resolver.build-exception: 'X::Placeholder::Block', placeholder => $name;
                return False;
            }
            elsif nqp::istype($block, RakuAST::Method::Initializer) {
                self.add-sorry:
                    $resolver.build-exception: 'X::Placeholder::Attribute', placeholder => $name;
                return False;
            }
            else {
                my $signature := $block.signature;
                if $signature && $signature.parameters-initialized {
                    # @_ and %_ are only real placeholders if they were not
                    # already defined in the signature, so we need to check
                    # there before pulling the plug
                    if $name eq '@_' || $name eq '%_' {
                        return True if $signature.IMPL-HAS-PARAMETER($name) || $name eq '%_' && $method;
                    }
                    self.add-sorry:
                      $resolver.build-exception: 'X::Signature::Placeholder',
                        precursor   => 1,
                        placeholder => $name;
                    return False;
                }
                elsif nqp::istype($block, RakuAST::Method) && $lexical-name ne '%_' {
                    self.add-sorry:
                        $resolver.build-exception: 'X::AdHoc', :payload(
                            "Placeholder variables (eg. $name) cannot be used in a method.\nPlease specify an explicit signature, like {$method.declarator} {$method.name.canonicalize} ($lexical-name) \{ ... \}");
                    return False
                }
            }
        }
        else {
            self.add-sorry:
                $resolver.build-exception: 'X::Placeholder::Mainline', placeholder => $name;
            return False;
        }
        True
    }

    method IMPL-ALREADY-DECLARED(Bool $declared) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Placeholder, '$!already-declared', $declared ?? True !! False);
    }

    # The declaration of the parameter the owner generated for this name.
    # Analysis and emission follow it in the placeholder's stead.
    method IMPL-LOWERING-DECLARATION() {
        return $!lowering-declaration if nqp::isconcrete($!lowering-declaration);
        return nqp::null() unless nqp::isconcrete($!owner);
        my $signature := nqp::getattr($!owner, RakuAST::PlaceholderParameterOwner,
            '$!placeholder-signature');
        return nqp::null() unless nqp::isconcrete($signature);
        my str $name := self.lexical-name;
        for self.IMPL-UNWRAP-LIST($signature.parameters) {
            my $target := $_.target;
            if nqp::istype($target, RakuAST::ParameterTarget::Var)
                && $target.name eq $name
                && nqp::isconcrete($target.declaration) {
                nqp::bindattr(self, RakuAST::VarDeclaration::Placeholder,
                    '$!lowering-declaration', $target.declaration);
                return $target.declaration;
            }
        }
        nqp::null()
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my $declaration := self.IMPL-LOWERING-DECLARATION;
        my str $local-name := nqp::isconcrete($declaration)
            ?? $declaration.IMPL-LOWERED-LOCAL-NAME
            !! '';
        $local-name
            ?? QAST::Var.new( :name($local-name), :scope('local') )
            !! QAST::Var.new( :name(self.lexical-name), :scope('lexical') )
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $declaration := self.IMPL-LOWERING-DECLARATION;
        nqp::isconcrete($declaration) && $declaration.IMPL-LOWERED-LOCAL-NAME
            ?? $declaration.IMPL-QAST-DECL($context)
            !! QAST::Var.new( :decl('var'), :scope('lexical'), :name(self.lexical-name) )
    }
}

# A positional placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::Positional
  is RakuAST::VarDeclaration::Placeholder
{
    has str $.lexical-name;

    method new(str $declared-name) {
        if nqp::substr($declared-name, 1, 1) eq '^' {
            nqp::die('Should construct a ' ~ self.HOW.name(self) ~ ' without the ^ twigil');
        }
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Placeholder::Positional,
            '$!lexical-name', $declared-name);
        $obj
    }

    method declared-name() {
        nqp::replace(self.lexical-name, 1, 0, '^')
    }

    method generate-parameter() {
        RakuAST::Parameter.new:
          target => RakuAST::ParameterTarget::Var.new(:name(self.lexical-name))
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $name := self.declared-name;

        if nqp::chars($name) == 3 && nqp::substr($name, 2, 1) ~~ /^<[A..Z]>$/ {
            self.add-sorry:
                $resolver.build-exception: 'X::Syntax::Perl5Var', :$name;
        }

        nqp::findmethod(RakuAST::VarDeclaration::Placeholder, 'PERFORM-CHECK')(self, $resolver, $context);
    }
}

# A named placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::Named
  is RakuAST::VarDeclaration::Placeholder
{
    has str $.lexical-name;

    method new(str $declared-name) {
        if nqp::substr($declared-name, 1, 1) eq ':' {
            nqp::die('Should construct a ' ~ self.HOW.name(self) ~ ' without the : twigil');
        }
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Placeholder::Named,
            '$!lexical-name', $declared-name);
        $obj
    }

    method declared-name() {
        nqp::replace(self.lexical-name, 1, 0, ':')
    }

    method generate-parameter() {
        my str $name := self.lexical-name;
        RakuAST::Parameter.new:
          target   => RakuAST::ParameterTarget::Var.new(:$name),
          names    => [nqp::substr($name,1)],
          optional => False
    }
}

# base class for slurpy placeholders
class RakuAST::VarDeclaration::Placeholder::Slurpy
  is RakuAST::VarDeclaration::Placeholder
{
    method new() {
        nqp::create(self)
    }

    method generate-parameter() {
        RakuAST::Parameter.new:
          target => RakuAST::ParameterTarget::Var.new(:name(self.lexical-name)),
          slurpy => RakuAST::Parameter::Slurpy::Flattened
    }
}

# A slurpy array placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::SlurpyArray
  is RakuAST::VarDeclaration::Placeholder::Slurpy
{
    method lexical-name() { '@_' }
}

# A slurpy hash placeholder parameter.
class RakuAST::VarDeclaration::Placeholder::SlurpyHash
  is RakuAST::VarDeclaration::Placeholder::Slurpy
{
    method lexical-name() { '%_' }
}
