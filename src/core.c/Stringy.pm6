my class X::NYI { ... }

my role Stringy { }

multi sub infix:<eqv>(Stringy:D \a, Stringy:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
        || (nqp::eqaddr(a.WHAT,b.WHAT) && nqp::iseq_i(a cmp b,0))
    )
}

proto sub prefix:<~>($, *%) is pure {*}
multi sub prefix:<~>(\a)          { a.Stringy }
multi sub prefix:<~>(int $a)      { nqp::p6box_s($a) }
multi sub prefix:<~>(num $a)      { nqp::p6box_s($a) }

proto sub infix:<~>(|) is pure {*}
multi sub infix:<~>(--> '') { }
multi sub infix:<~>($x     --> Str:D) { $x.Stringy }

proto sub infix:<x>($?, $?, *%) is pure {*}
multi sub infix:<x>() { Failure.new("No zero-arg meaning for infix:<x>") }
multi sub infix:<x>($x)            { $x.Stringy }
multi sub infix:<x>($s, Num:D $n) {
    $n == Inf
      ?? Failure.new(X::NYI.new(:feature('Cat object')))
      !! $s.Stringy x $n.Int;
}
multi sub infix:<x>($s, Any:D $n) { $s.Stringy x $n.Int         }
multi sub infix:<x>($s, Any:U $n) { $s.Stringy x $n.Numeric.Int }

proto sub infix:<eq>($?, $?, *%)  is pure {*}
multi sub infix:<eq>($x?)          { Bool::True }
multi sub infix:<eq>(\a, \b)       { a.Stringy eq b.Stringy }

proto sub infix:<ne>(Mu $?, Mu $?, *%) is pure {*}
multi sub infix:<ne>($x?)            { Bool::True }
multi sub infix:<ne>(Mu \a, Mu \b)   { a !eq b }
multi sub infix:<ne>(Any \a, Any \b) { a.Stringy ne b.Stringy }

proto sub infix:<lt>($?, $?, *%) is pure {*}
multi sub infix:<lt>($x?)          { Bool::True }
multi sub infix:<lt>(\a, \b)       { a.Stringy lt b.Stringy }

proto sub infix:<le>($?, $?, *%) is pure {*}
multi sub infix:<le>($x?)          { Bool::True }
multi sub infix:<le>(\a, \b)       { a.Stringy le b.Stringy }

proto sub infix:<gt>($?, $?, *%) is pure {*}
multi sub infix:<gt>($x?)          { Bool::True }
multi sub infix:<gt>(\a, \b)       { a.Stringy gt b.Stringy }

proto sub infix:<ge>($?, $?, *%) is pure {*}
multi sub infix:<ge>($x?)          { Bool::True }
multi sub infix:<ge>(\a, \b)       { a.Stringy ge b.Stringy }

proto sub infix:<~|>($?, $?, *%) is pure {*}
multi sub infix:<~|>($x = '')      { $x.Stringy }
multi sub infix:<~|>(\a, \b)       { a.Stringy ~| b.Stringy }

proto sub infix:<~^>($?, $?, *%)  is pure {*}
multi sub infix:<~^>($x = '')      { $x.Stringy }
multi sub infix:<~^>(\a, \b)       { a.Stringy ~^ b.Stringy }

proto sub infix:<~&>($?, $?, *%) is pure {*}
multi sub infix:<~&>() { Failure.new("No zero-arg meaning for infix:<~&>") }
multi sub infix:<~&>($x)           { $x.Stringy }
multi sub infix:<~&>(\a, \b)       { a.Stringy ~& b.Stringy }

proto sub prefix:<~^>($, *%) is pure {*}
multi sub prefix:<~^>(\a)         { ~^ a.Stringy }

# vim: expandtab shiftwidth=4
