## miscellaneous operators can go here.
##   generic numeric operators are in Numeric.pm
##   generic string operators are in Stringy.pm
##   Int/Rat/Num operators are in {Int|Rat|Num}.pm

sub infix:<=>(Mu \$a, Mu \$b) is rw {
    pir::perl6_container_store__0PP($a, $b)
}

sub SEQUENCE($left, $right, :$exclude_end) {
    my @right := $right.flat;
    my $endpoint = @right.shift;
    my $infinite = $endpoint ~~ Whatever;
    $endpoint = Bool::False if $infinite;
    my $tail := ().list;

    my sub generate($code) {
        my $count = $code.count;
        my $value;
        while 1 {
            $tail.munch($tail.elems - $count);
            $value := $code(|$tail);
            last if $value ~~ $endpoint;
            $tail.push($value);
            take $value;
        }
        $value;
    }

    my sub succpred($cmp) {
        ($cmp < 0) ?? { $^x.succ } !! ( $cmp > 0 ?? { $^x.pred } !! { $^x } )
    }

    (GATHER({
        my @left := $left.flat;
        my $value;
        my $code;
        my $stop;
        while @left {
            $value = @left.shift;
            if $value ~~ Code { $code = $value; last }
            if $value ~~ $endpoint { $stop = 1; last }
            $tail.push($value);
            take $value;
        }
        unless $stop {
            $tail.munch($tail.elems - 3) if $tail.elems > 3;
            my $a = $tail[0];
            my $b = $tail[1];
            my $c = $tail[2];
            if $code.defined { }
            elsif $tail.elems == 3 {
                my $ab = $b - $a;
                if $ab == $c - $b {
                    if $ab != 0 || $a ~~ Numeric && $b ~~ Numeric && $c ~~ Numeric {
                        $code = { $^x + $ab } 
                    }
                    else {
                        $code = succpred($b cmp $c)
                    }
                }
                elsif $a != 0 && $b != 0 && $c != 0 {
                    $ab = $b / $a;
                    if $ab == $c / $b {
                        $code = { $^x * $ab }
                    }
                }
            }
            elsif $tail.elems == 2 {
                my $ab = $b - $a;
                if $ab != 0 || $a ~~ Numeric && $b ~~ Numeric { 
                    $code = { $^x + $ab } 
                }
                else {
                    $code = succpred($a cmp $b)
                }
            }
            elsif $tail.elems == 1 {
                $code = $a cmp $endpoint > 0 ?? { $^x.pred } !! { $^x.succ }
            }
            elsif $tail.elems == 0 {
                $code = {()}
            }
            fail "unable to deduce sequence" unless $code.defined;
            $value = generate($code);
        }
        take $value unless $exclude_end;
    }, :$infinite), @right).list;
}

# XXX Wants to be a macro when we have them.
sub WHAT(\$x) {
    $x.WHAT
}

proto sub infix:<...>(|$) { * }
multi sub infix:<...>($a, $b) { SEQUENCE($a, $b) }

proto sub infix:<...^>(|$) { * }
multi sub infix:<...^>($a, $b) { SEQUENCE($a, $b, :exclude_end(1)) }

sub undefine(Mu \$x) {
    my $undefined;
    $x = $undefined;
}
