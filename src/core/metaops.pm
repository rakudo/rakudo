
sub METAOP_ASSIGN(\$op) {
    -> Mu \$a, Mu \$b { $a = $op($a, $b) }
}

sub METAOP_NEGATE(\$op) {
    -> Mu \$a, Mu \$b { !$op($a,$b) }
}

sub METAOP_REVERSE(\$op) {
    -> Mu \$a, Mu \$b { $op($b, $a) }
}

sub METAOP_CROSS(\$op) {
    fail "X metaoperator NYI";
}

sub METAOP_ZIP(\$op) {
    fail "Z metaoperator NYI";
}

sub METAOP_REDUCE(\$op, :$triangle) {
    my $x :=
    sub (*@values) {
        if $triangle {
            GATHER({
                return unless @values;
                my $result := @values.shift;
                take $result;
                take ($result := $op($result, @values.shift))
                    while @values;
            }, :infinite(@values.infinite))
        }
        else {
            return $op() unless @values;
            my $result := @values.shift;
            return $op($result) unless @values;
            $result := $op($result, @values.shift)
                while @values;
            $result;
        }
    }
}

sub METAOP_REDUCE_RIGHT(\$op, :$triangle) {
    my $x :=
    sub (*@values) {
        my $list = @values.reverse;
        if $triangle {
            gather {
                return unless $list;
                my $result := $list.shift;
                take $result;
                take ($result := $op($list.shift, $result))
                    while $list;
            }
        }
        else {
            return $op() unless $list;
            my $result := $list.shift;
            return $op($result) unless $list;
            $result := $op($list.shift, $result)
                while $list;
            $result;
        }
    }
}


sub METAOP_REDUCE_CHAIN(\$op, :$triangle) {
    fail "chaining reduce NYI";
}


sub METAOP_REDUCE_XOR(\$op, :$triangle) {
    fail "xor reduce NYI";
}

