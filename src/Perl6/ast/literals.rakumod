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
    method QAST {
        my $value := $!value;
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
    method QAST {
        my $value := $!value;
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
    method QAST {
        my $value := $!value;
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
    method QAST {
        my $value := $!value;
        QAST::WVal.new( :$value )
    }
}
