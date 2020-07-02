class RakuAST::IntLiteral is RakuAST::Term {
    has Int $.value;
    
    method new(Int $value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::IntLiteral, '$!value', $value);
        $obj
    }

    method type {
        $!value.WHAT
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        my $wval := QAST::WVal.new( :$value );
        nqp::isbig_I($value)
            ?? $wval
            !! QAST::Want.new( $wval, 'Ii', QAST::IVal.new( :value(nqp::unbox_i($value)) ) )
    }
}

class RakuAST::NumLiteral is RakuAST::Term {
    has Num $.value;

    method new(Num $value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::NumLiteral, '$!value', $value);
        $obj
    }

    method type {
        $!value.WHAT
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        my $wval := QAST::WVal.new( :$value );
        QAST::Want.new( $wval, 'Nn', QAST::NVal.new( :value(nqp::unbox_n($value)) ) )
    }
}

class RakuAST::RatLiteral is RakuAST::Term {
    has Rat $.value;

    method new(Rat $value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::RatLiteral, '$!value', $value);
        $obj
    }

    method type {
        $!value.WHAT
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }
}

class RakuAST::VersionLiteral is RakuAST::Term {
    has Any $.value;

    method new($value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::VersionLiteral, '$!value', $value);
        $obj
    }

    method type {
        $!value.WHAT
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }
}

# A StrLiteral is a basic string literal without any kind of interpolation
# taking place. It may be placed in the tree directly, but a compiler will
# typically emit it in a quoted string wrapper.
class RakuAST::StrLiteral is RakuAST::Term {
    has Str $.value;

    method new(Str $value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StrLiteral, '$!value', $value);
        $obj
    }

    method type {
        $!value.WHAT
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        my $wval := QAST::WVal.new( :$value );
        QAST::Want.new( $wval, 'Ss', QAST::SVal.new( :value(nqp::unbox_s($value)) ) )
    }
}

# A quoted string consists of a sequence of segments that should be evaluated
# (if needed) and concatenated. Processing may be applied to the result (these
# are "words", "quotewords", "val", and "exec", and are applied in the order
# that they are specified here).
class RakuAST::QuotedString is RakuAST::Term is RakuAST::ImplicitLookups {
    has Mu $!segments;
    has Mu $!processors;

    method new(List :$segments!, List :$processors) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::QuotedString, '$!segments',
            self.IMPL-UNWRAP-LIST($segments));
        nqp::bindattr($obj, RakuAST::QuotedString, '$!processors',
            self.IMPL-CHECK-PROCESSORS($processors));
        $obj
    }

    method IMPL-CHECK-PROCESSORS(Mu $processors) {
        my constant VALID := nqp::hash('words', Mu, 'quotewords', Mu, 'val', Mu, 'exec', 'Mu');
        my @result;
        for self.IMPL-UNWRAP-LIST($processors // []) {
            if nqp::existskey(VALID, $_) {
                nqp::push(@result, nqp::box_s($_, Str));
            }
            else {
                nqp::die("Unsupported quoted string processor '$_'");
            }
        }
        @result
    }

    method segments() {
        self.IMPL-WRAP-LIST($!segments)
    }

    method processors() {
        self.IMPL-WRAP-LIST($!processors)
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @needed;
        if nqp::elems($!processors) {
            for $!processors {
                if $_ eq 'val' {
                    nqp::push(@needed, RakuAST::Var::Lexical.new('&val'));
                    last;
                }
            }
        }
        self.IMPL-WRAP-LIST(@needed)
    }

    method type {
        # XXX depends on processors
        Str
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        if nqp::elems($!segments) == 1 {
            my $optimized := self.IMPL-TO-QAST-LITERAL($context);
            return $optimized if $optimized;
        }
        my @segment-asts;
        for $!segments {
            if $_.type =:= Str {
                @segment-asts.push($_.IMPL-TO-QAST($context));
            }
            else {
                my $inter-qast := $_.IMPL-TO-QAST($context);
                if nqp::istype($_, RakuAST::Block) {
                    $inter-qast := QAST::Op.new( :op('call'), $inter-qast );
                }
                @segment-asts.push(QAST::Op.new(
                    :op('callmethod'), :name('Str'),
                    $inter-qast
                ));
            }
        }
        my int $seg-count := nqp::elems(@segment-asts);
        my $qast;
        if $seg-count == 1 {
            $qast := @segment-asts[0];
        }
        elsif $seg-count == 2 {
            $qast := QAST::Op.new( :op('concat'), @segment-asts[0], @segment-asts[1] )
        }
        else {
            $qast := QAST::Op.new(
                :op('join'),
                QAST::SVal.new( :value('') ),
                QAST::Op.new( :op('list_s'), |@segment-asts )
            )
        }
        self.IMPL-QAST-PROCESSORS($context, $qast)
    }

    method IMPL-TO-QAST-LITERAL(RakuAST::IMPL::QASTContext $context) {
        # When we have a single piece and it's a literal string, we may be able
        # to optimize processing of it better.
        my $seg := $!segments[0];
        if nqp::istype($seg, RakuAST::StrLiteral) {
            # TODO
        }

        Nil
    }

    method IMPL-QAST-PROCESSORS(RakuAST::IMPL::QASTContext $context, Mu $qast) {
        # Non-optimized handling of processors.
        for $!processors {
            if $_ eq 'words' {
                $qast := QAST::Op.new(
                    :op('callmethod'), :name('WORDS_AUTODEREF'), $qast
                );
            }
            elsif $_ eq 'val' {
                my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
                $qast := QAST::Op.new(
                    :op('call'), :name(@lookups[0].resolution.lexical-name), $qast
                );
            }
            elsif $_ eq 'exec' {
                $qast := QAST::Op.new(
                    :op('call'), :name('&QX'), $qast
                );
            }
            else {
                nqp::die("Compiling processor '$_' is not implemented");
            }
        }
        $qast
    }

    method visit-children(Code $visitor) {
        my @segments := $!segments;
        for @segments {
            $visitor($_);
        }
    }
}

# An atom in a quote words construct. By wrapping something in this, it is
# considered indivisible in words processing.
class RakuAST::QuoteWordsAtom is RakuAST::Node {
    has RakuAST::Node $.atom;

    method new(RakuAST::Node $atom) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::QuoteWordsAtom, '$!atom', $atom);
        $obj
    }

    method type {
        $!atom.type
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!atom.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!atom)
    }
}
