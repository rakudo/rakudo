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
# (if needed) and concatenated.
class RakuAST::QuotedString is RakuAST::Term {
    has Mu $!segments;

    method new(*@segments) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::QuotedString, '$!segments', @segments);
        $obj
    }

    method segments() {
        self.IMPL-WRAP-LIST($!segments)
    }

    method type {
        Str
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my @segment-asts;
        for $!segments {
            if $_.type =:= Str {
                @segment-asts.push($_.IMPL-TO-QAST($context));
            }
            else {
                @segment-asts.push(QAST::Op.new(
                    :op('callmethod'), :name('Str'),
                    $_.IMPL-TO-QAST($context)
                ));
            }
        }
        my int $seg-count := nqp::elems(@segment-asts);
        if $seg-count == 1 {
            @segment-asts[0]
        }
        elsif $seg-count == 2 {
            QAST::Op.new( :op('concat'), @segment-asts[0], @segment-asts[1] )
        }
        else {
            QAST::Op.new(
                :op('join'),
                QAST::SVal.new( :value('') ),
                QAST::Op.new( :op('list_s'), |@segment-asts )
            )
        }
    }

    method visit-children(Code $visitor) {
        my @segments := $!segments;
        for @segments {
            $visitor($_);
        }
    }
}
