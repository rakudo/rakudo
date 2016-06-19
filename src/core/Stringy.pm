my class X::NYI { ... }

my role Stringy { }

multi sub infix:<eqv>(Stringy:D \a, Stringy:D \b) {
    ?(a =:= b || (a.WHAT =:= b.WHAT && (a cmp b) == 0))  # XXX RT #128092
}

proto sub prefix:<~>($) is pure { * }
multi sub prefix:<~>(\a)          { a.Stringy }
multi sub prefix:<~>(int $a)      { nqp::p6box_s($a) }
multi sub prefix:<~>(num $a)      { nqp::p6box_s($a) }

proto sub infix:<~>(|) is pure { * }
multi sub infix:<~>($x = '')       { $x.Stringy }
multi sub infix:<~>(\a, \b)        { a.Stringy ~ b.Stringy }

proto sub infix:<x>(Mu $?, Mu $?)  is pure { * }
multi sub infix:<x>() { Failure.new("No zero-arg meaning for infix:<x>") }
multi sub infix:<x>($x)            { $x.Stringy }
multi sub infix:<x>($s, Num:D $n) {
    $n == Inf
      ?? Failure.new(X::NYI.new(:feature('Cat object')))
      !! $s.Stringy x $n.Int;
}
multi sub infix:<x>($s, $n)        { $s.Stringy x ($n.Int // 0) }

proto sub infix:<leg>(Mu $?, Mu $?) is pure { * }
multi sub infix:<leg>(\a, \b)      { a.Stringy cmp b.Stringy }

proto sub infix:<eq>(Mu $?, Mu $?)  is pure { * }
multi sub infix:<eq>($x?)          { Bool::True }
multi sub infix:<eq>(\a, \b)       { a.Stringy eq b.Stringy }

proto sub infix:<ne>(Mu $?, Mu $?) is pure { * }
multi sub infix:<ne>($x?)            { Bool::True }
multi sub infix:<ne>(Mu \a, Mu \b)   { a !eq b }
multi sub infix:<ne>(Any \a, Any \b) { a.Stringy ne b.Stringy }

proto sub infix:<lt>(Mu $?, Mu $?) is pure { * }
multi sub infix:<lt>($x?)          { Bool::True }
multi sub infix:<lt>(\a, \b)       { a.Stringy lt b.Stringy }

proto sub infix:<le>(Mu $?, Mu $?) is pure { * }
multi sub infix:<le>($x?)          { Bool::True }
multi sub infix:<le>(\a, \b)       { a.Stringy le b.Stringy }

proto sub infix:<gt>(Mu $?, Mu $?) is pure { * }
multi sub infix:<gt>($x?)          { Bool::True }
multi sub infix:<gt>(\a, \b)       { a.Stringy gt b.Stringy }

proto sub infix:<ge>(Mu $?, Mu $?) is pure { * }
multi sub infix:<ge>($x?)          { Bool::True }
multi sub infix:<ge>(\a, \b)       { a.Stringy ge b.Stringy }

proto sub infix:<~|>(Mu $?, Mu $?) is pure { * }
multi sub infix:<~|>($x = '')      { $x.Stringy }
multi sub infix:<~|>(\a, \b)       { a.Stringy ~| b.Stringy }

proto sub infix:<~^>(Mu $?, Mu $?)  is pure { * }
multi sub infix:<~^>($x = '')      { $x.Stringy }
multi sub infix:<~^>(\a, \b)       { a.Stringy ~^ b.Stringy }

proto sub infix:<~&>(Mu $?, Mu $?) is pure { * }
multi sub infix:<~&>() { Failure.new("No zero-arg meaning for infix:<~&>") }
multi sub infix:<~&>($x)           { $x.Stringy }
multi sub infix:<~&>(\a, \b)       { a.Stringy ~& b.Stringy }

proto sub prefix:<~^>(Mu $) is pure { * }
multi sub prefix:<~^>(\a)         { ~^ a.Stringy }

# vim: ft=perl6 expandtab sw=4
