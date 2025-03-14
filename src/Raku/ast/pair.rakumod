# Base role done by things that serve as named arguments.
class RakuAST::NamedArg
  is RakuAST::Node
{
    method named-arg-name() { nqp::die('named-arg-name not implemented') }
    method named-arg-value() { nqp::die('named-arg-value not implemented') }
}

# A fat arrow pair, such as `foo => 42`.
class RakuAST::FatArrow
  is RakuAST::Term
  is RakuAST::ImplicitLookups
  is RakuAST::NamedArg
{
    has Str $.key;
    has RakuAST::Term $.value;

    method new(Str :$key!, RakuAST::Term :$value!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::FatArrow, '$!key', $key);
        nqp::bindattr($obj, RakuAST::FatArrow, '$!value', $value);
        $obj
    }

    method set-key(Str $key) {
        nqp::bindattr(self, RakuAST::FatArrow, '$!key', $key);
    }

    method named-arg-name() { $!key }

    method named-arg-value() { $!value }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair')),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $pair-type :=
          self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value;
        my $key := $!key;
        $context.ensure-sc($key);
        QAST::Op.new(
            :op('callmethod'), :name('new'),
            QAST::WVal.new( :value($pair-type) ),
            QAST::WVal.new( :value($key) ),
            $!value.IMPL-TO-QAST($context)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!value);
    }
}

# The base of all colonpair like constructs that can be added to a name.
class RakuAST::ColonPairish {
    method IMPL-QUOTE-VALUE($v) {
        if $v ~~ /<[ < > ]>/ && !($v ~~ /<[ « » $ \\ " ' ]>/) {
            '«' ~ $v ~ '»'
        }
        else {
            my $new := '';
            my int $e := nqp::chars($v);
            my int $i;
            while $i < $e {
                my $ch := nqp::substr($v,$i,1);
                $new := $new ~ '\\' if $ch eq '<' || $ch eq '>';
                $new := $new ~ $ch;
                ++$i;
            }
            '<' ~ $new ~ '>';
        }
    }
}

# The base of all colonpair constructs.
class RakuAST::ColonPair
  is RakuAST::ColonPairish
  is RakuAST::Term
  is RakuAST::ImplicitLookups
  is RakuAST::NamedArg
{
    has Str $.key;

    method value() { nqp::die(self.HOW.name(self) ~ ' does not implement value') }

    method set-key(Str $key) {
        nqp::bindattr(self, RakuAST::ColonPair, '$!key', $key);
    }

    method named-arg-name() { $!key }

    method named-arg-value() { self.value }

    method properties() { OperatorProperties.postfix(':') }

    method canonicalize() {
        my $value := self.simple-compile-time-quote-value;
        $value := self.value.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new)
            if !$value && self.value.IMPL-CAN-INTERPRET;
        $!key ~ (
            $value
                ?? self.IMPL-QUOTE-VALUE($value)
                !! self.value.origin
                    ?? self.value.origin.Str
                    !! self.value.DEPARSE)
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair')),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $pair-type :=
          self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value;
        my $key := $!key;
        $context.ensure-sc($key);
        QAST::Op.new(
            :op('callmethod'), :name('new'),
            QAST::WVal.new( :value($pair-type) ),
            QAST::WVal.new( :value($key) ),
            self.IMPL-VALUE-QAST($context)
        )
    }

    method IMPL-VALUE-QAST(RakuAST::IMPL::QASTContext $context) {
        self.value.IMPL-TO-QAST($context)
    }
}

# Stacked colonpairs, e.g. :a:b:c which should really be a list of colonpairs
# that gets interpolated into the surrounding list (e.g. arglist)
class RakuAST::ColonPairs
    is RakuAST::Node
{
    has Mu $.colonpairs;

    method new(*@pairs) {
        my $obj   := nqp::create(self);
        my $first := @pairs.shift;

        if nqp::istype($first,RakuAST::ColonPairs) {
            my $colonpairs := $first.colonpairs;
            while @pairs {
                nqp::push($colonpairs,nqp::shift(@pairs));
            }
            nqp::bindattr($obj,RakuAST::ColonPairs,'$!colonpairs',$colonpairs);
        }
        else {
            nqp::unshift(@pairs,$first);
            nqp::bindattr($obj,RakuAST::ColonPairs,'$!colonpairs',@pairs);
        }
        $obj
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $qast := QAST::Op.new(:op<call>, :name<&infix:<,>>);
        for self.colonpairs {
            $qast.push($_.IMPL-EXPR-QAST($context))
        }
        $qast
    }
}

# The base of colonpairs that can be placed as adverbs on a quote construct.
class RakuAST::QuotePair
  is RakuAST::ColonPair { }

# A truthy colonpair (:foo).
class RakuAST::ColonPair::True
  is RakuAST::QuotePair
{
    method new(Str $key) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ColonPair, '$!key', $key);
        $obj
    }

    method canonicalize() {
        nqp::getattr(self, RakuAST::ColonPair, '$!key')
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair'))
        ])
    }

    method value() {
        RakuAST::Declaration::ResolvedConstant.new(compile-time-value => True)
    }

    method simple-compile-time-quote-value() { True }

    method IMPL-VALUE-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := True;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() { True }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value.new(
          self.key, True
        )
    }
}

# A falsey colonpair (:!foo).
class RakuAST::ColonPair::False
  is RakuAST::QuotePair
{
    method new(Str $key) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ColonPair, '$!key', $key);
        $obj
    }

    method canonicalize() {
        "!" ~ nqp::getattr(self, RakuAST::ColonPair, '$!key')
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair'))
        ])
    }

    method value() {
        RakuAST::Declaration::ResolvedConstant.new(compile-time-value => False)
    }

    method simple-compile-time-quote-value() { False }

    method IMPL-VALUE-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := False;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() { True }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value.new(
          self.key, False
        )
    }
}

# A number colonpair (:2th).
class RakuAST::ColonPair::Number
  is RakuAST::QuotePair
{
    has RakuAST::IntLiteral $.value;

    method new(Str :$key!, RakuAST::IntLiteral :$value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ColonPair, '$!key', $key);
        nqp::bindattr($obj, RakuAST::ColonPair::Number, '$!value', $value);
        $obj
    }

    method simple-compile-time-quote-value() { $!value.value }

    method visit-children(Code $visitor) {
        $visitor($!value);
    }

    method IMPL-CAN-INTERPRET() { True }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value.new(
          self.key, $!value.IMPL-INTERPRET($ctx)
        )
    }
}

# A colonpair with a value (:foo(42)).
class RakuAST::ColonPair::Value
  is RakuAST::QuotePair
{
    has RakuAST::Expression $.value;

    method new(Str :$key!, RakuAST::Expression :$value!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ColonPair, '$!key', $key);
        nqp::bindattr($obj, RakuAST::ColonPair::Value, '$!value', $value);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!value);
    }

    method simple-compile-time-quote-value() {
        # TODO various cases we can handle here
        if nqp::istype(self.value, RakuAST::QuotedString) {
            self.value.literal-value
        }
        else {
            Nil
        }
    }

    method IMPL-CAN-INTERPRET() { $!value.IMPL-CAN-INTERPRET }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value.new(
          self.key, $!value.IMPL-INTERPRET($ctx)
        )
    }
}

# A variable colonpair (:$var, :$<match>).
class RakuAST::ColonPair::Variable
  is RakuAST::ColonPair
{
    has RakuAST::Var $.value;

    method new(Str :$key!, RakuAST::Var :$value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ColonPair, '$!key', $key);
        nqp::bindattr($obj, RakuAST::ColonPair::Variable, '$!value', $value);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!value);
    }
}
