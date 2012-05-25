## miscellaneous operators can go here.
##   generic numeric operators are in Numeric.pm
##   generic string operators are in Stringy.pm
##   Int/Rat/Num operators are in {Int|Rat|Num}.pm

sub infix:<=>(Mu \$a, Mu \$b) is rw {
    pir::perl6_container_store__0PP($a, $b)
}

proto infix:<does>(|$) { * }
multi infix:<does>(Mu:D \$obj, Mu:U \$role) is rw {
    # XXX Mutability check.
    $obj.HOW.mixin($obj, $role).BUILD_LEAST_DERIVED({});
}
multi infix:<does>(Mu:D \$obj, Mu:U \$role, :$value! is parcel) is rw {
    # XXX Mutability check.
    my @attrs = $role.^attributes().grep: { .has_accessor };
    die(X::Role::Initialization.new())
        unless @attrs == 1;
    $obj.HOW.mixin($obj, $role).BUILD_LEAST_DERIVED({ @attrs[0].Str.substr(2) => $value });
}
multi infix:<does>(Mu:U \$obj, Mu:U \$role) is rw {
    die(X::Does::TypeObject.new())
}
multi infix:<does>(Mu:D \$obj, @roles) is rw {
    # XXX Mutability check.
    $obj.HOW.mixin($obj, |@roles).BUILD_LEAST_DERIVED({});
}
multi infix:<does>(Mu:U \$obj, @roles) is rw {
    die(X::Does::TypeObject.new())
}

proto infix:<but>(|$) { * }
multi infix:<but>(Mu:D \$obj, Mu:U \$role) {
    $obj.HOW.mixin($obj.clone(), $role).BUILD_LEAST_DERIVED({});
}
multi infix:<but>(Mu:D \$obj, Mu:U \$role, :$value! is parcel) {
    my @attrs = $role.^attributes().grep: { .has_accessor };
    die(X::Role::Initialization.new())
        unless @attrs == 1;
    $obj.HOW.mixin($obj.clone(), $role).BUILD_LEAST_DERIVED({ @attrs[0].Str.substr(2) => $value });
}
multi infix:<but>(Mu:U \$obj, Mu:U \$role) {
    $obj.HOW.mixin($obj, $role);
}
multi infix:<but>(Mu \$obj, Mu:D $val) is rw {
    my $role := Metamodel::ParametricRoleHOW.new_type();
    my $meth := method () { $val };
    $meth.set_name($val.^name);
    $role.HOW.add_method($role, $meth.name, $meth);
    $role.HOW.set_body_block($role,
        -> |$c { nqp::list($role, nqp::hash('$?CLASS', $c<$?CLASS>)) });
    $role.HOW.compose($role);
    $obj.HOW.mixin($obj.clone(), $role);
}
multi infix:<but>(Mu:D \$obj, @roles) {
    $obj.HOW.mixin($obj.clone(), |@roles).BUILD_LEAST_DERIVED({});
}
multi infix:<but>(Mu:U \$obj, @roles) {
    $obj.HOW.mixin($obj, |@roles)
}

sub SEQUENCE($left, $right, :$exclude_end) {
    my @right := $right.flat;
    my $endpoint = @right.shift;
    my $infinite = $endpoint ~~ Whatever;
    $endpoint = Bool::False if $infinite;
    my $tail := ().list;

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

            if $code.defined {
                my $count = $code.count;
                while 1 {
                    $tail.munch($tail.elems - $count);
                    $value := $code(|$tail);
                    last if $value ~~ $endpoint;
                    $tail.push($value);
                    take $value;
                }
            }
            else {
                $value = (sub { fail X::Sequence::Deduction.new })();
            }
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

sub infix:<ff>($a as Bool, $b as Bool) {
    my $pos := nqp::p6box_s(nqp::callerid());
    state %ffv;
    if %ffv{$pos} {
        %ffv{$pos} = False if $b;
        True;
    }
    elsif $a {
        %ffv{$pos} = $a
    }
    else {
        False
    }
}

sub infix:<ff^>($a as Bool, $b as Bool) {
    my $pos := nqp::p6box_s(nqp::callerid());
    state %ffv;
    if %ffv{$pos} {
        $b ?? (%ffv{$pos} = False) !! True
    }
    elsif $a {
        %ffv{$pos} = $a
    }
    else {
        False
    }
}

sub infix:<^ff>($a as Bool, $b as Bool) {
    my $pos := nqp::p6box_s(nqp::callerid());
    state %ffv;
    if %ffv{$pos} {
        %ffv{$pos} = False if $b;
        True
    }
    else {
        %ffv{$pos} = True if $a;
        False
    }
}

sub infix:<^ff^>($a as Bool, $b as Bool) {
    my $pos := nqp::p6box_s(nqp::callerid());
    state %ffv;
    if %ffv{$pos} {
        $b ?? (%ffv{$pos} = False) !! True
    }
    else {
        %ffv{$pos} = True if $a;
        False
    }
}

# not sure where this should go
# this implements the ::() indirect lookup
sub INDIRECT_NAME_LOOKUP($root, *@chunks) is rw {
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
    my Mu $thing := $root.exists($first) ?? $root{$first} !!
                    GLOBAL::.exists($first) ?? GLOBAL::{$first} !!
                    fail("Symbol '$name' not found");
    for @parts {
        fail("Symbol '$name not found") unless $thing.WHO.exists($_);
        $thing := $thing.WHO{$_};
    }
    $thing;
}
