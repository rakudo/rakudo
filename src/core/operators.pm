## miscellaneous operators can go here.
##   generic numeric operators are in Numeric.pm
##   generic string operators are in Stringy.pm
##   Int/Rat/Num operators are in {Int|Rat|Num}.pm

sub infix:<=>(Mu \a, Mu \b) is raw {
    nqp::p6store(a, b)
}

my class X::Does::TypeObject is Exception {
    has Mu $.type;
    method message() { "Cannot use 'does' operator with a type object." }
}

proto sub infix:<does>(|) { * }
multi sub infix:<does>(Mu:D \obj, Mu:U \rolish) is raw {
    # XXX Mutability check.
    my $role := rolish.HOW.archetypes.composable() ?? rolish !!
                rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
                X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw;
    obj.^mixin($role).BUILD_LEAST_DERIVED({});
}
multi sub infix:<does>(Mu:D \obj, Mu:U \rolish, :$value! is raw) is raw {
    # XXX Mutability check.
    my $role := rolish.HOW.archetypes.composable() ?? rolish !!
                rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
                X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw;
    my \mixedin = obj.^mixin($role, :need-mixin-attribute);
    mixedin.BUILD_LEAST_DERIVED({ substr(mixedin.^mixin_attribute.Str,2) => $value });
}
multi sub infix:<does>(Mu:U \obj, Mu:U \role) is raw {
    X::Does::TypeObject.new(type => obj).throw
}
multi sub infix:<does>(Mu:D \obj, **@roles) is raw {
    # XXX Mutability check.
    my \real-roles = eager @roles.map: -> \rolish {
        rolish.HOW.archetypes.composable() ?? rolish !!
            rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
            X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw
    }
    obj.^mixin(|real-roles).BUILD_LEAST_DERIVED({});
}
multi sub infix:<does>(Mu:U \obj, **@roles) is raw {
    X::Does::TypeObject.new(type => obj).throw
}

# we need this candidate tighter than infix:<cmp>(Real:D, Real:D)
# but can't yet use `is default` at the place where that candidate
# is defined because it uses `infix:<does>`
multi sub infix:<cmp>(Rational:D \a, Rational:D \b) is default {
    a.Num cmp b.Num
}

proto sub infix:<but>(|) is pure { * }
multi sub infix:<but>(Mu:D \obj, Mu:U \rolish) {
    my $role := rolish.HOW.archetypes.composable() ?? rolish !!
                rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
                X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw;
    obj.clone.^mixin($role).BUILD_LEAST_DERIVED({});
}
multi sub infix:<but>(Mu:D \obj, Mu:U \rolish, :$value! is raw) {
    my $role := rolish.HOW.archetypes.composable() ?? rolish !!
                rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
                X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw;
    my \mixedin = obj.clone.^mixin($role, :need-mixin-attribute);
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
    obj.^mixin($role);
}
sub GENERATE-ROLE-FROM-VALUE($val) {
    my $role := Metamodel::ParametricRoleHOW.new_type();
    my $meth := method () { $val };
    $meth.set_name($val.^name);
    $role.^add_method($meth.name, $meth);
    $role.^set_body_block(
      -> |c { nqp::list($role, nqp::hash('$?CLASS', c<$?CLASS>)) });
    $role.^compose;
}
multi sub infix:<but>(Mu \obj, Mu:D $val) is raw {
    obj.clone.^mixin(GENERATE-ROLE-FROM-VALUE($val));
}
multi sub infix:<but>(Mu:D \obj, **@roles) {
    my \real-roles := eager @roles.map: -> \rolish {
        rolish.DEFINITE ?? GENERATE-ROLE-FROM-VALUE(rolish) !!
            rolish.HOW.archetypes.composable() ?? rolish !!
            rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
            X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw
    }
    obj.clone.^mixin(|real-roles).BUILD_LEAST_DERIVED({});
}
multi sub infix:<but>(Mu:U \obj, **@roles) {
    my \real-roles := eager @roles.map: -> \rolish {
        rolish.DEFINITE ?? GENERATE-ROLE-FROM-VALUE(rolish) !!
            rolish.HOW.archetypes.composable() ?? rolish !!
            rolish.HOW.archetypes.composalizable() ?? rolish.HOW.composalize(rolish) !!
            X::Mixin::NotComposable.new(:target(obj), :rolish(rolish)).throw
    }
    obj.^mixin(|real-roles)
}

sub SEQUENCE(\left, Mu \right, :$exclude_end) {
    my \righti := nqp::iscont(right)
        ?? right.iterator
        !! [right].iterator;
    my $endpoint := righti.pull-one;
    X::Cannot::Empty.new(:action('get sequence endpoint'), :what('list (use * or :!elems instead?)')).throw
      if $endpoint =:= IterationEnd;
    $endpoint.sink if $endpoint ~~ Failure;
    my $infinite = nqp::istype($endpoint,Whatever) || $endpoint === Inf;
    $endpoint := Bool::False if $infinite;
    my @tail;
    my $end_code_arity = 0;
    my @end_tail;
    if nqp::istype($endpoint,Code) && !nqp::istype($endpoint,Regex) {
        $end_code_arity = $endpoint.arity;
        $end_code_arity = $endpoint.count if $end_code_arity == 0;
        $end_code_arity = -Inf if $end_code_arity == Inf;
    }

    my sub succpred($a,$b) {
        my $cmp = $a cmp $b;
        if $a.WHAT === $b.WHAT === $endpoint.WHAT {
            $cmp < 0 && $a ~~ Stringy
                ?? -> $x {
                    my $new = $x.succ;
                    last if $new after $endpoint or $new.chars > $endpoint.chars;
                    $new;
                }
                !! $cmp < 0
                    ?? -> $x {
                        my $new = $x.succ;
                        last if $new after $endpoint;
                        $new;
                    }
                    !! $cmp > 0
                        ?? -> $x {
                            my $new = $x.pred;
                            last if $x before $endpoint;
                            $new;
                        }
                        !! { $_ }
        }
        else {
            $cmp < 0
                ?? { $^x.succ }
                !! $cmp > 0
                    ?? { $^x.pred }
                    !! { $^x }
        }
    }
    my sub unisuccpred($a,$b) {
        my $cmp = $a.ord cmp $b.ord;
        $cmp < 0
            ?? { $^x.ord.succ.chr }
            !! $cmp > 0
                ?? { $^x.ord.pred.chr }
                !! { $^x }
    }

    my \gathered = GATHER({
        my \lefti := left.iterator;
        my $value;
        my $code;
        my $stop;
        my $looped;
        while !((my \value := lefti.pull-one) =:= IterationEnd) {
            $looped = True;
            if nqp::istype(value,Code) { $code = value; last }
            if $end_code_arity != 0 {
                @end_tail.push(value);
                if +@end_tail >= $end_code_arity {
                    @end_tail.shift xx (@end_tail.elems - $end_code_arity) unless $end_code_arity ~~ -Inf;
                    if $endpoint(|@end_tail) {
                        $stop = 1;
                        @tail.push(value) unless $exclude_end;
                        last;
                    }
                }
            }
            elsif value ~~ $endpoint {
                $stop = 1;
                @tail.push(value) unless $exclude_end;
                last;
            }
            @tail.push(value);
        }
        X::Cannot::Empty.new(:action('get sequence start value'), :what('list')).throw
          unless $looped;
        if $stop {
            take $_ for @tail;
        }
        else {
            my $badseq;
            my $a;
            my $b;
            my $c;
            unless $code.defined {
                take @tail.shift while @tail.elems > 3;
                $a = @tail[0];
                $b = @tail[1];
                $c = @tail[2];
            }
            if $code.defined { }
            elsif @tail.grep(Real).elems != @tail.elems {
                if @tail.elems > 1 {
                    if @tail.tail.WHAT === $endpoint.WHAT {
                        $code = succpred(@tail.tail, $endpoint);
                    }
                    else {
                        $code = succpred(@tail[*-2], @tail.tail);
                    }
                }
                elsif nqp::istype($endpoint, Stringy) and nqp::istype($a, Stringy) and nqp::isconcrete($endpoint) {
                    if $a.codes == 1 && $endpoint.codes == 1 {
                        $code = unisuccpred($a, $endpoint);
                    }
                    elsif $a.codes == $endpoint.codes {
                        my @a = $a.comb;
                        my @e = $endpoint.comb;
                        my @ranges;
                        for flat @a Z @e -> $from, $to {
                            @ranges.push: $($from ... $to);
                        }
                        .take for flat [X~] @ranges;
                        $stop = 1;
                    }
                    elsif $a lt $endpoint {
                        $stop = 1 if $a gt $endpoint;
                        $code = -> $x {
                            my $new = $x.succ;
                            last if $new gt $endpoint or $new.chars > $endpoint.chars;
                            $new;
                        }
                    }
                    else {
                        $stop = 1 if $a lt $endpoint;
                        $code = -> $x {
                            my $new = $x.pred;
                            last if $new lt $endpoint;
                            $new;
                        }
                    }
                }
                elsif $infinite or nqp::istype($endpoint, Code) {
                    $code = *.succ;
                }
                else {
                    $code = succpred($a,$endpoint);
                }
            }
            elsif @tail.elems == 3 {
                my $ab = $b - $a;
                if $ab == $c - $b {
                    if $ab != 0 || nqp::istype($a,Real) && nqp::istype($b,Real) && nqp::istype($c,Real) {
                        if nqp::istype($endpoint, Real) and not nqp::istype($endpoint, Bool) and nqp::isconcrete($endpoint) {
                            if $ab > 0 {
                                $stop = 1 if $a > $endpoint;
                                $code = -> $x {
                                    my $new = $x + $ab;
                                    last if $new > $endpoint;
                                    $new;
                                }
                            }
                            else {
                                $stop = 1 if $a < $endpoint;
                                $code = -> $x {
                                    my $new = $x + $ab;
                                    last if $new < $endpoint;
                                    $new;
                                }
                            }
                        }
                        else {
                            $code = { $^x + $ab }
                        }
                    }
                    else {
                        $code = succpred($b, $c)
                    }
                }
                elsif $a != 0 && $b != 0 && $c != 0 {
                    $ab = $b / $a;
                    if $ab == $c / $b {
                        $ab = $ab.Int if nqp::istype($ab,Rat) && $ab.denominator == 1;
                        if nqp::istype($endpoint, Real) and not nqp::istype($endpoint, Bool) and nqp::isconcrete($endpoint) {
                            if $ab > 0 {
                                if $ab > 1  {
                                    $stop = 1 if $a > $endpoint;
                                    $code = -> $x {
                                        my $new = $x * $ab;
                                        last if $new > $endpoint;
                                        $new;
                                    }
                                }
                                else {
                                    $stop = 1 if $a < $endpoint;
                                    $code = -> $x {
                                        my $new = $x * $ab;
                                        last if $new < $endpoint;
                                        $new;
                                    }
                                }
                            }
                            else {
                                $code = -> $x {
                                    my $new = $x * $ab;
                                    my $absend = $endpoint.abs;
                                    last if sign($x.abs - $absend) == -sign($new.abs - $absend);
                                    $new;
                                }
                            }
                        }
                        else {
                            $code = { $^x * $ab }
                        }
                    }
                }
                if $code {
                    @tail.pop;
                    @tail.pop;
                }
                else {
                    $badseq = "$a,$b,$c" unless $code;
                }
            }
            elsif @tail.elems == 2 {
                my $ab = $b - $a;
                if $ab != 0 || nqp::istype($a,Real) && nqp::istype($b,Real) {
                    if nqp::istype($endpoint, Real) and not nqp::istype($endpoint, Bool) and nqp::isconcrete($endpoint) {
                        if $ab > 0 {
                            $stop = 1 if $a > $endpoint;
                            $code = -> $x {
                                my $new = $x + $ab;
                                last if $new > $endpoint;
                                $new;
                            }
                        }
                        else {
                            $stop = 1 if $a < $endpoint;
                            $code = -> $x {
                                my $new = $x + $ab;
                                last if $new < $endpoint;
                                $new;
                            }
                        }
                    }
                    else {
                        $code = { $^x + $ab }
                    }
                }
                else {
                    $code = succpred($a, $b)
                }
                @tail.pop;
            }
            elsif @tail.elems == 1 {
                if nqp::istype($endpoint,Code) or not nqp::isconcrete($endpoint) {
                    $code = { $^x.succ }
                }
                elsif nqp::istype($endpoint, Real) and not nqp::istype($endpoint, Bool) and nqp::istype($a, Real) {
                    if $a < $endpoint {
                        $code = -> $x {
                            my $new = $x.succ;
                            last if $new > $endpoint;
                            $new;
                        }
                    }
                    else {
                        $code = -> $x {
                            my $new = $x.pred;
                            last if $new < $endpoint;
                            $new;
                        }
                    }
                }
                else {
                    $code = { $^x.succ }
                }
            }
            elsif @tail.elems == 0 {
                $code = {()}
            }

            if $stop { }
            elsif $code.defined {
                .take for @tail;
                my $count = $code.count;

                until $stop {
                    @tail.shift while @tail.elems > $count;
                    my \value = $code(|@tail);
                    if $end_code_arity != 0 {
                        @end_tail.push(value);
                        if @end_tail.elems >= $end_code_arity {
                            @end_tail.shift xx (@end_tail.elems - $end_code_arity) unless $end_code_arity == -Inf;
                            if $endpoint(|@end_tail) {
                                value.take unless $exclude_end;
                                $stop = 1;
                            }
                        }
                    }
                    elsif value ~~ $endpoint {
                        value.take unless $exclude_end;
                        $stop = 1;
                    }

                    if $stop { }
                    else {
                        @tail.push(value);
                        value.take;
                    }
                }
            }
            elsif $badseq {
                die X::Sequence::Deduction.new(:from($badseq));
            }
            else {
                die X::Sequence::Deduction.new();
            }
        }
    });
    $infinite
        ?? (gathered.Slip, Slip.from-iterator(righti)).lazy
        !! (gathered.Slip, Slip.from-iterator(righti))
}

# XXX Wants to be macros when we have them.
sub WHAT(Mu \x) { x.WHAT }
sub HOW (Mu \x) { x.HOW }
sub VAR (Mu \x) { x.VAR }

proto sub infix:<...>(|) { * }
multi sub infix:<...>(\a, Mu \b) { Seq.new(SEQUENCE(a, b).iterator) }
multi sub infix:<...>(|lol) {
    my @lol := lol.list;
    my @end;
    my @seq;
    my @excl;
    my $ret := ();
    my int $i = 0;
    my int $m = +@lol - 1;
    while $i <= $m {
        @seq[$i] := @lol[$i].iterator;
        if $i {
            @end[$i-1] := @seq[$i].pull-one;
            if @end[$i-1] ~~ Numeric | Stringy {
                @seq[$i] := @lol[$i].iterator;
                @excl[$i-1] = True;
            }
        }
        ++$i;
    }
    $i = 0;
    while $i < $m {
        $ret := ($ret.Slip,
            SEQUENCE(
                (Slip.from-iterator(@seq[$i]),),
                @end[$i],
                :exclude_end(so @excl[$i])
            ).Slip
        );
        ++$i;
    }
    if @seq[$m] =:= Empty {
        Seq.new($ret.iterator);
    }
    else {
        Seq.new(($ret.Slip, Slip.from-iterator(@seq[$m])).iterator);
    }
}

proto sub infix:<...^>(|) { * }
multi sub infix:<...^>(\a, Mu \b) { Seq.new(SEQUENCE(a, b, :exclude_end(1)).iterator) }

proto sub infix:<…>(|) { * }
multi sub infix:<…>(|c) { infix:<...>(|c) }

proto sub infix:<…^>(|) { * }
multi sub infix:<…^>(|c) { infix:<...^>(|c) }

multi sub undefine(Mu \x) is raw { x = Nil }
multi sub undefine(Array \x) is raw { x = Empty }
multi sub undefine(Hash \x) is raw { x = Empty }

sub prefix:<temp>(\cont) is raw {
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

sub prefix:<let>(\cont) is raw {
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

# this implements the ::() indirect lookup
sub INDIRECT_NAME_LOOKUP($root, *@chunks) is raw {
    nqp::if(
      # Note that each part of @chunks itself can contain double colons.
      # That's why joining and re-splitting is necessary
      nqp::elems(my $parts :=
        nqp::split('::',my str $name = @chunks.join('::'))),
      nqp::stmts(
        (my str $first = nqp::shift($parts)),
        nqp::if(
          nqp::elems($parts),
          nqp::stmts(
            (my str $sigil = nqp::substr($first,0,1)),
            nqp::if(
              nqp::iseq_s($sigil,'$')
                || nqp::iseq_s($sigil,'@')
                || nqp::iseq_s($sigil,'%')
                || nqp::iseq_s($sigil,'&'),
              nqp::stmts(
                nqp::push($parts,
                  nqp::concat($sigil,nqp::unbox_s(nqp::pop($parts)))),
                ($first = nqp::substr($first,1))
              )
            ),
            nqp::unless(
              $first,
              nqp::stmts(
                ($first = nqp::shift($parts)),
                ($name  = nqp::join("::",$parts)),
              )
            )
          )
        ),
        (my Mu $thing := nqp::if(
          $root.EXISTS-KEY($first),
          $root.AT-KEY($first),
          nqp::if(
            GLOBAL::.EXISTS-KEY($first),
            GLOBAL::.AT-KEY($first),
            X::NoSuchSymbol.new(symbol => $name).fail
          )
        )),
        nqp::while(
          nqp::elems($parts),
          nqp::if(
            $thing.WHO.EXISTS-KEY(my $part := nqp::shift($parts)),
            ($thing := $thing.WHO.AT-KEY($part)),
            X::NoSuchSymbol.new(symbol => $name).fail
          )
        ),
        $thing
      ),
      X::NoSuchSymbol.new(symbol => $name).fail
    )
}

sub REQUIRE_IMPORT($compunit, *@syms) {
    my $handle := $compunit.handle;
    my $DEFAULT := $handle.export-package()<DEFAULT>.WHO;
    my $GLOBALish := $handle.globalish-package;
    my @missing;
    # Set the runtime values for compile time stub symbols
    for @syms {
        unless $DEFAULT.EXISTS-KEY($_) {
            @missing.push: $_;
            next;
        }
        OUTER::CALLER::{$_} := $DEFAULT{$_};
    }
    if @missing {
        X::Import::MissingSymbols.new(:from($compunit.short-name), :@missing).throw;
    }
    # Merge GLOBAL from compunit.
    GLOBAL::.merge-symbols($GLOBALish);
    Nil;
}

sub infix:<andthen>(+a) {
    my $ai := a.iterator;
    my Mu $current := $ai.pull-one;
    return Bool::True if $current =:= IterationEnd;
    nqp::until(
        (($_ := $ai.pull-one) =:= IterationEnd),
        nqp::stmts(
            (return Empty unless $current.defined),
            ($current := $_ ~~ Callable
                ?? (.count ?? $_($current) !! $_())
                !! $_
            ),
        ),
        :nohandler, # do not handle control stuff in thunks
    );
    $current;
}
sub infix:<notandthen>(+a) {
    my $ai := a.iterator;
    my Mu $current := $ai.pull-one;
    return Bool::True if $current =:= IterationEnd;
    nqp::until(
        (($_ := $ai.pull-one) =:= IterationEnd),
        nqp::stmts(
            (return Empty if $current.defined),
            ($current := $_ ~~ Callable
                ?? (.count ?? $_($current) !! $_())
                !! $_
            ),
        ),
        :nohandler, # do not handle control stuff in thunks
    );
    $current;
}

sub infix:<orelse>(+a) {
    my $ai := a.iterator;
    my Mu $current := $ai.pull-one;
    return Nil if $current =:= IterationEnd;

    # Flag for heuristic when we were passed an Empty as LHS
    my int $handle-empty = 1;
    nqp::until(
        (($_ := $ai.pull-one) =:= IterationEnd),
        nqp::stmts(
            (return $current if $current.defined),
            ($handle-empty = 0),
            ($current := $_ ~~ Callable
                ?? (.count ?? $_($current) !! $_())
                !! $_
            ),
        ),
        :nohandler, # do not handle control stuff in thunks
    );

    if $handle-empty and $current ~~ Callable {
        $_ := Empty; # set $_ in the Callable
        $current := $current.count ?? $current($_) !! $current();
    }

    $current;
}

# next three sub would belong to traits.pm if PseudoStash were available
# so early in the setting compunit
multi sub trait_mod:<is>(Routine $r, Str :$equiv!) {
    if (my $i = nqp::index($r.name, ':')) > 0 {
        my \nm ='&' ~ nqp::substr($r.name, 0, $i+1) ~ '<' ~ nqp::escape($equiv) ~ '>';
        trait_mod:<is>($r, equiv => ::(nm));
        return;
    }
    die "Routine given to equiv does not appear to be an operator";;
}

multi sub trait_mod:<is>(Routine $r, Str :$tighter!) {
    if (my $i = nqp::index($r.name, ':')) > 0 {
        my \nm ='&' ~ nqp::substr($r.name, 0, $i+1) ~ '<' ~ nqp::escape($tighter) ~ '>';
        trait_mod:<is>($r, tighter => ::(nm));
        return;
    }
    die "Routine given to tighter does not appear to be an operator";;
}

multi sub trait_mod:<is>(Routine $r, Str :$looser!) {
    if (my $i = nqp::index($r.name, ':')) > 0 {
        my \nm ='&' ~ nqp::substr($r.name, 0, $i+1) ~ '<' ~ nqp::escape($looser) ~ '>';
        trait_mod:<is>($r, looser => ::(nm));
        return;
    }
    die "Routine given to looser does not appear to be an operator";;
}

proto sub infix:<∘> (&?, &?) {*}
multi sub infix:<∘> () { *.self }
multi sub infix:<∘> (&f) { &f }
multi sub infix:<∘> (&f, &g --> Block) { (&f).count > 1 ?? -> |args { f |g |args } !! -> |args { f g |args } }
my &infix:<o> := &infix:<∘>;

# vim: ft=perl6 expandtab sw=4
