## miscellaneous operators can go here.
##   generic numeric operators are in Numeric.pm
##   generic string operators are in Stringy.pm
##   Int/Rat/Num operators are in {Int|Rat|Num}.pm

sub infix:<=>(Mu \$a, Mu \$b) is rw {
    pir::perl6_container_store__0PP($a, $b)
}

proto infix:<does>(|$) { * }
multi infix:<does>(Mu \$obj, Mu:U \$role) is rw {
    # XXX Mutability check.
    $obj.HOW.mixin($obj, $role);
}
multi infix:<does>(Mu \$obj, @roles) is rw {
    # XXX Mutability check.
    $obj.HOW.mixin($obj, |@roles);
}

proto infix:<but>(|$) { * }
multi infix:<but>(Mu \$obj, Mu:U \$role) {
    $obj.HOW.mixin($obj.clone(), $role);
}
multi infix:<but>(Mu \$obj, @roles) {
    $obj.HOW.mixin($obj.clone(), |@roles);
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
                        $ab = $ab.Int if $ab ~~ Rat && $ab.denominator == 1;
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
            $value = $code.defined 
                       ?? generate($code) 
                       !! (sub { fail "unable to deduce sequence" })();
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

# not sure where this should go
# this implements the ::() indirect lookup
sub INDIRECT_NAME_LOOKUP(*@chunks) is rw {
    # note that each part of @chunks itself can
    # contain double colons. That's why joining and
    # re-splitting is necessary
    my Str $name = @chunks.join('::');
    my @parts    = $name.split('::');
    my $first    = @parts.shift;
    if @parts && '$@%&'.index($first.substr(0, 1)).defined {
        # move sigil from first to last chunk, because
        # $Foo::Bar::baz is actually stored as Foo::Bar::$baz
        my $last_idx      = @parts.end;
        @parts[$last_idx] = $first.substr(0, 1) ~ @parts[$last_idx]; 
        $first            = $first.substr(1);
        if $first eq '' {
            $first = @parts.shift;
            $name = @chunks.join('::');
        }
    }
    my Mu $thing := pir::find_caller_lex__Ps(
        nqp::unbox_s($first)
    );
    fail("Symbol '$name' not found") if nqp::isnull($thing);
    for @parts {
        fail("Symbol '$name not found") unless $thing.WHO.exists($_);
        $thing := $thing.WHO{$_};
    }
    $thing;
}
