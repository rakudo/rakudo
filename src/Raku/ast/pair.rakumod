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
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair')),
        ]
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $pair-type :=
          self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value;
        my $key := $!key;
        $context.ensure-sc($key);
        QAST::Op.new(
            :op('callmethod'), :name('new'),
            QAST::WVal.new( :value($pair-type) ),
            QAST::WVal.new( :value($key) ),
            $!value.IMPL-TO-QAST($context)
        )
    }

    method IMPL-CAN-INTERPRET() { $!value.IMPL-CAN-INTERPRET }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value.new(
          $!key, $!value.IMPL-INTERPRET($ctx)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!value);
    }
}

# The base of all colonpair like constructs that can be added to a name.
class RakuAST::ColonPairish {
    method IMPL-QUOTE-VALUE($v) {
        if nqp::istype($v, List) {
            # In bootstrap List may not be able to stringify yet
            my $list := RakuAST::Node.IMPL-UNWRAP-LIST($v);
            $v := '';
            for $list {
                $v := $v ~ ' ' if $v;
                $v := $v ~ $_
            }
        }
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

    method simple-compile-time-quote-value() { Nil }

    # Failure results are returned, not rethrown; callers decide, see
    # `IMPL-EVAL-COLONPAIR-VALUE-OR-RETHROW`. Value is memoized on the
    # node so side effects fire at most once.
    method IMPL-EVAL-COLONPAIR-VALUE(RakuAST::Resolver $resolver,
                                     RakuAST::IMPL::QASTContext $context) {
        self.simple-compile-time-quote-value
    }

    # `$Failure` is the caller's already-resolved Failure type, or
    # `nqp::null` during CORE setting bootstrap.
    method IMPL-EVAL-COLONPAIR-VALUE-OR-RETHROW(RakuAST::Resolver $resolver,
                                                RakuAST::IMPL::QASTContext $context,
                                                Mu $Failure) {
        my $value := self.IMPL-EVAL-COLONPAIR-VALUE($resolver, $context);
        $value.exception.throw
            if !nqp::isnull($Failure) && nqp::istype($value, $Failure);
        $value
    }

    method canonicalize() {
        my $value := self.simple-compile-time-quote-value;
        $!key ~ (
            $value
                ?? self.IMPL-QUOTE-VALUE($value)
                !! self.value.origin
                    ?? self.value.origin.Str
                    !! self.value.DEPARSE)
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair')),
        ]
    }

    method IMPL-CREATE-PAIR(Str $key, Mu $value) {
        my $Pair := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value;
        my $pair := nqp::create($Pair);
        nqp::bindattr($pair, $Pair, '$!key', $key);
        nqp::bindattr($pair, $Pair, '$!value', $value);
        $pair
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $pair-type :=
          self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value;
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
    is RakuAST::Term
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

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
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
  is RakuAST::CompileTimeValue
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
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair'))
        ]
    }

    method value() {
        RakuAST::Declaration::ResolvedConstant.new(compile-time-value => True)
    }

    method simple-compile-time-quote-value() { True }

    method compile-time-value() {
        self.IMPL-CREATE-PAIR(self.key, True)
    }

    method IMPL-VALUE-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := True;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() { True }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value.new(
          self.key, True
        )
    }
}

# A falsey colonpair (:!foo).
class RakuAST::ColonPair::False
  is RakuAST::QuotePair
  is RakuAST::CompileTimeValue
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
        [
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Pair'))
        ]
    }

    method value() {
        RakuAST::Declaration::ResolvedConstant.new(compile-time-value => False)
    }

    method simple-compile-time-quote-value() { False }

    method compile-time-value() {
        self.IMPL-CREATE-PAIR(self.key, False)
    }

    method IMPL-VALUE-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := False;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method IMPL-CAN-INTERPRET() { True }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value.new(
          self.key, False
        )
    }
}

# A number colonpair (:2th).
class RakuAST::ColonPair::Number
  is RakuAST::QuotePair
  is RakuAST::CompileTimeValue
{
    has RakuAST::IntLiteral $.value;

    method new(Str :$key!, RakuAST::IntLiteral :$value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ColonPair, '$!key', $key);
        nqp::bindattr($obj, RakuAST::ColonPair::Number, '$!value', $value);
        $obj
    }

    method simple-compile-time-quote-value() { $!value.value }

    method compile-time-value() {
        self.IMPL-CREATE-PAIR(self.key, $!value.value)
    }

    method visit-children(Code $visitor) {
        $visitor($!value);
    }

    method IMPL-CAN-INTERPRET() { True }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value.new(
          self.key, $!value.IMPL-INTERPRET($ctx)
        )
    }
}

# A colonpair with a value (:foo(42)).
class RakuAST::ColonPair::Value
  is RakuAST::QuotePair
{
    has RakuAST::Expression $.value;
    has Mu  $!cached-value;
    has int $!has-cached-value;

    method new(Str :$key!, RakuAST::Expression :$value!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ColonPair, '$!key', $key);
        nqp::bindattr($obj, RakuAST::ColonPair::Value, '$!value', $value);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!value);
    }

    # Memoise on $!cached-value/$!has-cached-value; treat the flag as the
    # authoritative "is there a value" indicator so a legitimately falsy
    # interpret result (`:foo(0)`, `:foo("")`) doesn't get re-evaluated.
    method IMPL-CACHE-VALUE(Mu $value) {
        nqp::bindattr(self, RakuAST::ColonPair::Value, '$!cached-value', $value);
        nqp::bindattr_i(self, RakuAST::ColonPair::Value, '$!has-cached-value', 1);
        $value
    }

    # Never calls IMPL-INTERPRET, so callers that just need to introspect
    # (base canonicalize, adverb scanners) don't fire side effects.
    method simple-compile-time-quote-value() {
        return $!cached-value if $!has-cached-value;
        nqp::istype(self.value, RakuAST::QuotedString)
            ?? self.IMPL-CACHE-VALUE(self.value.literal-value)
            !! Nil
    }

    # Single source of truth for "interpret the value expression once and
    # cache it". Returns the interpreted value, or Nil when the expression
    # is not IMPL-INTERPRET-able and a BEGIN-time evaluation is needed
    # (only IMPL-EVAL-COLONPAIR-VALUE has the resolver/context to do that).
    method IMPL-INTERPRETED-VALUE-OR-NIL() {
        return $!cached-value if $!has-cached-value;
        my $v := self.simple-compile-time-quote-value;
        return $v if $!has-cached-value;
        $!value.IMPL-CAN-INTERPRET
            ?? self.IMPL-CACHE-VALUE($!value.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new))
            !! Nil
    }

    # Non-QuotedString values (`:foo['a','b']`, `:foo('a','b')`) need to
    # be interpreted so IMPL-QUOTE-VALUE can render the same `<a b>`
    # canonical form as `:foo<a b>`.
    method canonicalize() {
        my $value := self.IMPL-INTERPRETED-VALUE-OR-NIL;
        self.key ~ (
            $!has-cached-value && nqp::isconcrete($value)
                ?? self.IMPL-QUOTE-VALUE($value)
                !! $!value.origin
                    ?? $!value.origin.Str
                    !! $!value.DEPARSE)
    }

    method IMPL-EVAL-COLONPAIR-VALUE(RakuAST::Resolver $resolver,
                                     RakuAST::IMPL::QASTContext $context) {
        my $value := self.IMPL-INTERPRETED-VALUE-OR-NIL;
        return $value if $!has-cached-value;
        # Not IMPL-INTERPRET-able; fall through to BEGIN-time evaluation.
        # The CATCH re-attaches the colonpair's source location because
        # IMPL-BEGIN-TIME-EVALUATE attributes through self.origin, which
        # is unset on the BeginTime type object we dispatch through.
        {
            CATCH {
                my $ex := $resolver.convert-begin-time-exception($_);
                if nqp::can($ex, 'SET_FILE_LINE') && my $origin := self.origin {
                    my $origin-match := $origin.as-match;
                    $ex.SET_FILE_LINE($origin-match.file, $origin-match.line);
                }
                $ex.rethrow;
            }
            $value := RakuAST::BeginTime.IMPL-BEGIN-TIME-EVALUATE(
                $!value, $resolver, $context);
        }
        self.IMPL-CACHE-VALUE($value)
    }

    method has-compile-time-value() {
        $!value.has-compile-time-value
    }

    method maybe-compile-time-value() {
        self.IMPL-CREATE-PAIR(self.key, $!value.maybe-compile-time-value)
    }

    method IMPL-CAN-INTERPRET() { $!value.IMPL-CAN-INTERPRET }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value.new(
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
