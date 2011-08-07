
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
            return () unless @values;
            GATHER({
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
            return () unless $list;
            gather {
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

sub METAOP_HYPER(\$op, *%opt) {
    -> Mu \$a, Mu \$b { hyper($op, $a, $b, |%opt) }
}

sub METAOP_HYPER_POSTFIX(\$obj, \$op) { hyper($op, $obj) }

sub METAOP_HYPER_PREFIX(\$op, \$obj) { hyper($op, $obj) }

proto sub hyper(|$) { * }
multi sub hyper(\$op, \$a, \$b, :$dwim-left, :$dwim-right) { 
    my @alist := $a.elems < $b.elems && $dwim-left
                   ?? ($a xx *).munch($b.elems)
                   !! $a.flat;
    my @blist := $b.elems < $a.elems && $dwim-right
                   ?? ($b xx *).munch($a.elems)
                   !! $b.flat;
    die "Sorry, lists on both sides of non-dwimmy hyperop are not of same length:\n"
        ~ "    left: @alist.elems() elements, right: @blist.elems() elements\n"
      if @alist != @blist;

    (@alist Z @blist).map(
        -> $x, $y {
            Iterable.ACCEPTS($x)
              ?? $x.new(hyper($op, $x, $y, :$dwim-left, :$dwim-right)).item
              !! (Iterable.ACCEPTS($y)
                    ?? $y.new(hyper($op, $x, $y, :$dwim-left, :$dwim-right)).item
                    !! $op($x, $y))
        }
    )
}

multi sub hyper(\$op, \$a) {
    $a.map( { Iterable.ACCEPTS($_)
                ?? $_.new(hyper($op, $_)).item
                !! $op($_) } ).eager
}

multi sub hyper(\$op, %h) {
    hash %h.keys Z hyper($op, %h.values)
}

multi sub hyper(\$op, %a, %b, :$dwim-left, :$dwim-right) {
    my %k;
    for %a.keys { %k{$_} = 1 if !$dwim-left || %b.exists($_) }
    for %b.keys { %k{$_} = 1 if !$dwim-right }
    my @keys = %k.keys;
    hash @keys Z hyper($op, %a{@keys}, %b{@keys}, :$dwim-left, :$dwim-right)
}

multi sub hyper(\$op, %a, \$b, :$dwim-left, :$dwim-right) {
    hash %a.keys Z hyper($op, %a.values, $b, :$dwim-left, :$dwim-right);
}

multi sub hyper(\$op, \$a, %b, :$dwim-left, :$dwim-right) {
    hash %b.keys Z hyper($op, $a, %b.values, :$dwim-left, :$dwim-right);
}

