# Base role done by things that serve as named arguments.
class RakuAST::NamedArg is RakuAST::Node {
    method named-arg-name() { nqp::die('named-arg-name not implemented') }
    method named-arg-value() { nqp::die('named-arg-value not implemented') }
}

# A fat arrow pair, such as `foo => 42`.
class RakuAST::FatArrow is RakuAST::Term is RakuAST::ImplicitLookups is RakuAST::NamedArg {
    has Str $.key;
    has RakuAST::Term $.value;

    method new(Str :$key!, RakuAST::Term :$value!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::FatArrow, '$!key', $key);
        nqp::bindattr($obj, RakuAST::FatArrow, '$!value', $value);
        $obj
    }

    method named-arg-name() { $!key }

    method named-arg-value() { $!value }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Simple.new('Pair'),
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
        my $pair-type := @lookups[0].resolution.compile-time-value;
        my $key := $!key;
        $context.ensure-sc($key);
        QAST::Op.new(
            :op('callmethod'), :name('new'),
            QAST::WVal.new( :value($pair-type) ),
            QAST::WVal.new( :value($key) ),
            $!value.IMPL-TO-QAST($context)
        )
    }
}
