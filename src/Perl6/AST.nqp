# The Raku AST is the user-visible API to a program. The AST is primarily used
# when implementing macros. Since this is user-visible, all types descend from
# the Raku Any type. We wrap them up for intropsection purposes also.

# XXX Various things that are currently base classes would become roles.

stub RakuAST metaclass Perl6::Metamodel::PackageHOW { ... };
BEGIN { Perl6::Metamodel::PackageHOW.add_stash(RakuAST); }
stub RakuAST::Node metaclass Perl6::Metamodel::ClassHOW { ... };
stub RakuAST::IntLiteral metaclass Perl6::Metamodel::ClassHOW { ... };

BEGIN {
    ##
    ## Various utility subs to help us produce types that look Raku-like.
    ##

    sub parent($class, $parent) {
        $class.HOW.add_parent($class, $parent);
    }

    sub add-attribute($class, $type, $name) {
        $class.HOW.add_attribute($class, Attribute.new(
            :$name, :$type, :package($class)
        ));
    }

    sub add-method($class, $name, @parameters, $impl) {
        my $static-code := nqp::getstaticcode($impl);
        nqp::setcodename($static-code, $name);
        $class.HOW.add_method($class, $name, $static-code);
    }

    sub compose($type) {
        $type.HOW.compose_repr($type)
    }

    ##
    ## AST node definitions
    ##

    parent(RakuAST::Node, Any);
    add-method(RakuAST::Node, 'type', [], sub ($self) {
        Mu
    });
    # XXX temporary attributes/methods while we refactor to the new AST
    add-attribute(RakuAST::Node, Str, '$!named');
    add-method(RakuAST::Node, 'annotate', [], sub ($self, $key, $value) {
    });
    add-method(RakuAST::Node, 'ann', [], sub ($self, $key) {
        Mu
    });
    add-method(RakuAST::Node, 'wanted', [], sub ($self, $value?) {
        1
    });
    add-method(RakuAST::Node, 'sunk', [], sub ($self, $value?) {
        Mu
    });
    add-method(RakuAST::Node, 'returns', [], sub ($self, $value?) {
        $self.type
    });
    add-method(RakuAST::Node, 'has_compile_time_value', [], sub ($self) {
        0
    });
    add-method(RakuAST::Node, 'compile_time_value', [], sub ($self) {
        $self.value
    });
    add-method(RakuAST::Node, 'flat', [], sub ($self) {
        Mu
    });
    add-method(RakuAST::Node, 'named', [], sub ($self, $value?) {
       if nqp::isconcrete($value) {
            nqp::bindattr(nqp::decont($self), RakuAST::Node, '$!named', $value);
            $value
        }
        else {
            nqp::getattr(nqp::decont($self), RakuAST::Node, '$!named') || '';
        }
    });
    # XXX end temporaries
    compose(RakuAST::Node);

    parent(RakuAST::IntLiteral, RakuAST::Node);
    add-attribute(RakuAST::IntLiteral, Int, '$!value');
    add-method(RakuAST::IntLiteral, 'new', [Int, '$value'], sub ($self, $value) {
        my $obj := nqp::create($self);
        nqp::bindattr($obj, RakuAST::IntLiteral, '$!value', $value);
        $obj
    });
    add-method(RakuAST::IntLiteral, 'type', [], sub ($self) {
        nqp::getattr(nqp::decont($self), RakuAST::IntLiteral, '$!value').WHAT
    });
    add-method(RakuAST::IntLiteral, 'value', [], sub ($self) {
        nqp::getattr(nqp::decont($self), RakuAST::IntLiteral, '$!value')
    });
    add-method(RakuAST::IntLiteral, 'QAST', [], sub ($self) {
        my $value := nqp::getattr(nqp::decont($self), RakuAST::IntLiteral, '$!value');
        my $wval := QAST::WVal.new( :$value );
        nqp::isbig_I($value)
            ?? $wval
            !! QAST::Want.new( $wval, 'Ii', QAST::IVal.new( :value(nqp::unbox_i($!value)) ) )
    });
    compose(RakuAST::IntLiteral);
    
    ##
    ## Export of what we set up
    ##

    EXPORT::DEFAULT.WHO<RakuAST> := RakuAST;
}
