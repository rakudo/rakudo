# Marker for different variable-like things.
class RakuAST::Var is RakuAST::Term {
}

# A typical lexical variable lookup (e.g. $foo).
class RakuAST::Var::Lexical is RakuAST::Var is RakuAST::Lookup {
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Var::Lexical, '$!name', $name);
        $obj
    }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-lexical($!name);
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := self.resolution.lexical-name;
        QAST::Var.new( :$name, :scope<lexical> )
    }
}

# A regex positional capture variable (e.g. $0).
class RakuAST::Var::PositionalCapture is RakuAST::Var is RakuAST::ImplicitLookups {
    has Int $.index;

    method new(Int $index) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::PositionalCapture, '$!index', $index);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&postcircumfix:<[ ]>'),
            RakuAST::Var::Lexical.new('$/'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        QAST::Op.new(
            :op('call'),
            :name(@lookups[0].resolution.lexical-name),
            @lookups[1].IMPL-TO-QAST($context),
            QAST::WVal.new( :value($!index) )
        )
    }
}

# A regex named capture variable (e.g. $<foo>).
class RakuAST::Var::NamedCapture is RakuAST::Var is RakuAST::ImplicitLookups {
    has Str $.index;

    method new(Str $index) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Var::NamedCapture, '$!index', $index);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical.new('&postcircumfix:<{ }>'),
            RakuAST::Var::Lexical.new('$/'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        QAST::Op.new(
            :op('call'),
            :name(@lookups[0].resolution.lexical-name),
            @lookups[1].IMPL-TO-QAST($context),
            QAST::WVal.new( :value($!index) )
        )
    }
}
