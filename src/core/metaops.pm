
sub METAOP_ASSIGN(\$op) {
    -> Mu \$a, Mu \$b { $a = $op( $a // $op(), $b) }
}

sub METAOP_NEGATE(\$op) {
    -> Mu \$a, Mu \$b { !$op($a,$b) }
}

sub METAOP_REVERSE(\$op) {
    -> Mu \$a, Mu \$b { $op($b, $a) }
}

sub METAOP_CROSS(\$op) {
    -> **@lol {
        my $rop = METAOP_REDUCE($op);
        my @l;
        my @v;
        @l[0] = (@lol[0].flat,).list;
        my $i = 0;
        my $n = @lol.elems - 1;
        gather {
            while $i >= 0 {
                if @l[$i] {
                    @v[$i] = @l[$i].shift;
                    if $i >= $n { my @x = @v; take $rop(|@x); }
                    else {
                        $i++;
                        @l[$i] = (@lol[$i].flat,).list;
                    }
                }
                else { $i--; }
            }
        }
    }
}

sub METAOP_ZIP(\$op) {
    -> **@lol {
        my $rop = METAOP_REDUCE($op);
        my @l = @lol.map({ (.flat,).list.item });
        gather {
            my $loop = 1;
            while $loop {
                my @z = @l.map({ $loop = 0 unless $_; .shift });
                take $rop(|@z) if $loop;
            }
        }
    }
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
    NYI "chaining reduce NYI";
}


sub METAOP_REDUCE_XOR(\$op, :$triangle) {
    NYI "xor reduce NYI";
}


