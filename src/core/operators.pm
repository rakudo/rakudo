## miscellaneous operators can go here.
##   generic numeric operators are in Numeric.pm
##   generic string operators are in Stringy.pm
##   Int/Rat/Num operators are in {Int|Rat|Num}.pm

sub infix:<=>(Mu \a, Mu \b) is rw {
    nqp::p6store(a, b)
}

my class X::Does::TypeObject is Exception {
    has Mu $.type;
    method message() { "Cannot use 'does' operator with a type object." }
}

proto sub infix:<does>(Mu, Mu, *%) { * }
multi sub infix:<does>(Mu:D \obj, Mu:U \rolish) is rw {
    # XXX Mutability check.
    my $role := rolish.HOW.archetypes.composable() ?? rolish !!
                rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
                X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw;
    obj.HOW.mixin(obj, $role).BUILD_LEAST_DERIVED({});
}
multi sub infix:<does>(Mu:D \obj, Mu:U \rolish, :$value! is parcel) is rw {
    # XXX Mutability check.
    my $role := rolish.HOW.archetypes.composable() ?? rolish !!
                rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
                X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw;
    my \mixedin = obj.HOW.mixin(obj, $role, :need-mixin-attribute);
    mixedin.BUILD_LEAST_DERIVED({ substr(mixedin.^mixin_attribute.Str,2) => $value });
}
multi sub infix:<does>(Mu:U \obj, Mu:U \role) is rw {
    X::Does::TypeObject.new(type => obj).throw
}
multi sub infix:<does>(Mu:D \obj, @roles) is rw {
    # XXX Mutability check.
    obj.HOW.mixin(obj, |@roles).BUILD_LEAST_DERIVED({});
}
multi sub infix:<does>(Mu:U \obj, @roles) is rw {
    X::Does::TypeObject.new(type => obj).throw
}

proto sub infix:<but>(Mu, Mu, *%) { * }
multi sub infix:<but>(Mu:D \obj, Mu:U \rolish) {
    my $role := rolish.HOW.archetypes.composable() ?? rolish !!
                rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
                X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw;
    obj.HOW.mixin(obj.clone(), $role).BUILD_LEAST_DERIVED({});
}
multi sub infix:<but>(Mu:D \obj, Mu:U \rolish, :$value! is parcel) {
    my $role := rolish.HOW.archetypes.composable() ?? rolish !!
                rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
                X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw;
    my \mixedin = obj.HOW.mixin(obj.clone(), $role, :need-mixin-attribute);
    my \attr = mixedin.^mixin_attribute;
    my $mixin-value := $value;
    unless nqp::istype($value, attr.type) {
        if attr.type.HOW.^name eq 'Perl6::Metamodel::EnumHOW' {
            $mixin-value := attr.type.($value);
        }
    }
    mixedin.BUILD_LEAST_DERIVED({ substr(attr.Str,2) => $mixin-value });
}
multi sub infix:<but>(Mu:U \obj, Mu:U \rolish) {
    my $role := rolish.HOW.archetypes.composable() ?? rolish !!
                rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
                X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw;
    obj.HOW.mixin(obj, $role);
}
multi sub infix:<but>(Mu \obj, Mu:D $val) is rw {
    my $role := Metamodel::ParametricRoleHOW.new_type();
    my $meth := method () { $val };
    $meth.set_name($val.^name);
    $role.HOW.add_method($role, $meth.name, $meth);
    $role.HOW.set_body_block($role,
        -> |c { nqp::list($role, nqp::hash('$?CLASS', c<$?CLASS>)) });
    $role.HOW.compose($role);
    obj.HOW.mixin(obj.clone(), $role);
}
multi sub infix:<but>(Mu:D \obj, @roles) {
    obj.HOW.mixin(obj.clone(), |@roles).BUILD_LEAST_DERIVED({});
}
multi sub infix:<but>(Mu:U \obj, @roles) {
    obj.HOW.mixin(obj, |@roles)
}

sub SEQUENCE($left, Mu $right, :$exclude_end) {
    my @right := nqp::istype($right, Junction) || !$right.DEFINITE
      ?? [$right] !! $right.flat;
    my $endpoint = @right.shift;
    my $infinite = nqp::istype($endpoint,Whatever) || $endpoint === Inf;
    $endpoint = Bool::False if $infinite;
    my $tail := ().list;
    my $end_code_arity = 0;
    my $end_tail := ().list;
    if nqp::istype($endpoint,Code) {
        $end_code_arity = $endpoint.arity;
        $end_code_arity = $endpoint.count if $end_code_arity == 0;
        $end_code_arity = -Inf if $end_code_arity == Inf;
    }

    my sub succpred($cmp) {
        ($cmp < 0) ?? { $^x.succ } !! ( $cmp > 0 ?? { $^x.pred } !! { $^x } )
    }
    my sub unisuccpred($cmp) {
        ($cmp < 0) ?? { $^x.ord.succ.chr } !! ( $cmp > 0 ?? { $^x.ord.pred.chr } !! { $^x } )
    }

    (GATHER({
        my @left := $left.flat;
        my $value;
        my $code;
        my $stop;
        for @left -> $v {
            $value := $v;
            if nqp::istype($value,Code) { $code = $value; last }
            if $end_code_arity != 0 {
                $end_tail.push($value);
                if +@$end_tail >= $end_code_arity {
                    $end_tail.munch($end_tail.elems - $end_code_arity) unless $end_code_arity ~~ -Inf;
                    if $endpoint(|@$end_tail) { $stop = 1; last }
                }
            } elsif $value ~~ $endpoint  { $stop = 1; last }
            $tail.push($value);
            take $value;
        }
        unless $stop {
            my $badseq;
            my ($a, $b, $c);
            unless $code.defined {
                $tail.munch($tail.elems - 3) if $tail.elems > 3;
                $a = $tail[0];
                $b = $tail[1];
                $c = $tail[2];
            }
            if $code.defined { }
            elsif $tail.grep(Numeric).elems != $tail.elems {
                # non-numeric
                if $tail.elems == 1 {
                    if nqp::istype($a,Stringy) && nqp::istype($endpoint,Stringy) && $a.codes == 1 && $endpoint.codes == 1 {
                        $code = $infinite ?? { $^x.ord.succ.chr } !! unisuccpred($a.ord cmp $endpoint.ord);
                    } else {
                        $code = $infinite ??  { $^x.succ } !! succpred($a cmp $endpoint);
                    }
                }
                else {
                    $code = succpred($tail[*-2] cmp $tail[*-1]);
                }
            }
            elsif $tail.elems == 3 {
                my $ab = $b - $a;
                if $ab == $c - $b {
                    if $ab != 0 || nqp::istype($a,Numeric) && nqp::istype($b,Numeric) && nqp::istype($c,Numeric) {
                        $code = { $^x + $ab }
                    }
                    else {
                        $code = succpred($b cmp $c)
                    }
                }
                elsif $a != 0 && $b != 0 && $c != 0 {
                    $ab = $b / $a;
                    if $ab == $c / $b {
                        $ab = $ab.Int if nqp::istype($ab,Rat) && $ab.denominator == 1;
                        $code = { $^x * $ab }
                    }
                }
                $badseq = "$a,$b,$c" unless $code;
            }
            elsif $tail.elems == 2 {
                my $ab = $b - $a;
                if $ab != 0 || nqp::istype($a,Numeric) && nqp::istype($b,Numeric) {
                    $code = { $^x + $ab }
                }
                else {
                    $code = succpred($a cmp $b)
                }
            }
            elsif $tail.elems == 1 {
                $code = ($a cmp $endpoint > 0 && !nqp::istype($endpoint,Code))
                  ?? { $^x.pred }
                  !! { $^x.succ }
            }
            elsif $tail.elems == 0 {
                $code = {()}
            }

            if $code.defined {
                my $count = $code.count;
                while 1 {
                    $tail.munch($tail.elems - $count);
                    $value := $code(|$tail);
                    if $end_code_arity != 0 {
                        $end_tail.push($value);
                        if $end_tail.elems >= $end_code_arity {
                            $end_tail.munch($end_tail.elems - $end_code_arity) unless $end_code_arity == -Inf;
                            last if $endpoint(|@$end_tail);
                        }
                    } else {
                        last if $value ~~ $endpoint;
                    }
                    $tail.push($value);
                    take $value;
                }
            }
            elsif $badseq {
                $value := (sub { fail X::Sequence::Deduction.new(:from($badseq)) })();
            }
            else {
                $value := (sub { fail X::Sequence::Deduction.new() })();
            }
        }
        take $value unless $exclude_end;
    }, :$infinite), @right).list;
}

# XXX Wants to be a macro when we have them.
sub WHAT(Mu \x) { x.WHAT }
sub VAR (Mu \x) { x.VAR }

proto sub infix:<...>(|) { * }
multi sub infix:<...>($a, Mu $b) { SEQUENCE($a, $b) }
multi sub infix:<...>(**@lol) {
    my @ret;
    my int $i = 0;
    my int $m = +@lol - 1;
    my @tail = @lol[$m].list.splice(1); # trailing elems of last list added back later
    my $current_left;
    while $m > $i {
        if @ret { # 1st elem of left part can be closure; take computed value instead
            $current_left = (@ret.pop, @lol[$i].list.splice(1));
        }
        else { # no need to modify left part for first list
            $current_left = @lol[$i];
        }
        @ret := (@ret,
            SEQUENCE(
                $current_left,        # from-range (adjusted if needed), specifies steps
                @lol[$i + 1].list[0], # to, we only need the endpoint (= first item)
                :exclude_end( False ) # never exclude end; we take care of that
            )
        ).flat;
        $i = nqp::add_i($i, 1);
    }
    push @ret, @tail if @tail;  # add back trailing elements of last list
    @ret
}

proto sub infix:<...^>(|) { * }
multi sub infix:<...^>($a, Mu $b) { SEQUENCE($a, $b, :exclude_end(1)) }

sub undefine(Mu \x) { x = Nil }

sub prefix:<temp>(\cont) is rw {
    my $temp_restore := nqp::getlexcaller('!TEMP-RESTORE');
    my int $i = nqp::elems($temp_restore);
    while $i > 0 {
        $i = $i - 2;
        return-rw cont if nqp::atpos($temp_restore, $i) =:= cont;
    }
    if nqp::iscont(cont) {
        nqp::push($temp_restore, cont);
        nqp::push($temp_restore, nqp::decont(cont));
    }
    elsif nqp::istype(cont, Array) {
        nqp::push($temp_restore, cont);
        nqp::push($temp_restore, my @a = cont);
    }
    elsif nqp::istype(cont, Hash) {
        nqp::push($temp_restore, cont);
        nqp::push($temp_restore, my %h = cont);
    }
    else {
        X::Localizer::NoContainer.new(localizer => 'temp').throw;
    }
    cont
}

sub prefix:<let>(\cont) is rw {
    my $let_restore := nqp::getlexcaller('!LET-RESTORE');
    my int $i = nqp::elems($let_restore);
    while $i > 0 {
        $i = $i - 2;
        return-rw cont if nqp::atpos($let_restore, $i) =:= cont;
    }
    if nqp::iscont(cont) {
        nqp::push($let_restore, cont);
        nqp::push($let_restore, nqp::decont(cont));
    }
    elsif nqp::istype(cont, Array) {
        nqp::push($let_restore, cont);
        nqp::push($let_restore, my @a = cont);
    }
    elsif nqp::istype(cont, Hash) {
        nqp::push($let_restore, cont);
        nqp::push($let_restore, my %h = cont);
    }
    else {
        X::Localizer::NoContainer.new(localizer => 'let').throw;
    }
    cont
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
    if @parts && '$@%&'.index(substr($first,0, 1)).defined {
        # move sigil from first to last chunk, because
        # $Foo::Bar::baz is actually stored as Foo::Bar::$baz
        my $last_idx      = @parts.end;
        @parts[$last_idx] = substr($first,0, 1) ~ @parts[$last_idx];
        $first            = substr($first,1);
        if $first eq '' {
            $first = @parts.shift;
            $name = @chunks.join('::');
        }
    }
    my Mu $thing := $root.exists_key($first) ?? $root{$first} !!
                    GLOBAL::.exists_key($first) ?? GLOBAL::{$first} !!
                    X::NoSuchSymbol.new(symbol => $name).fail;
    for @parts {
        X::NoSuchSymbol.new(symbol => $name).fail unless $thing.WHO.exists_key($_);
        $thing := $thing.WHO{$_};
    }
    $thing;
}

sub REQUIRE_IMPORT($package-name, *@syms) {
    my $package = CALLER::OUR::($package-name);
    my $who     = $package.WHO;
    unless $who.exists_key('EXPORT') {
        die "Trying to import symbols @syms.join(', ') from '$package-name', but it does not export anything";
    }
    $who := $who<EXPORT>.WHO<DEFAULT>.WHO;
    my @missing;
    for @syms {
        unless $who.exists_key($_) {
            @missing.push: $_;
            next;
        }
        OUTER::CALLER::{$_} := $who{$_};
    }
    if @missing {
        X::Import::MissingSymbols.new(:from($package-name), :@missing).throw;
    }
    $package
}
sub infix:<andthen>(*@a) {
    my Mu $current := @a.shift;
    for @a {
        return $current unless $current.defined;
        $current := .count ?? $_(|$current) !! $_();
    }
    $current;
}

# vim: ft=perl6 expandtab sw=4
