my role Stringy { }

multi sub infix:<eqv>(Stringy:D $a, Stringy:D $b) {
    $a.WHAT === $b.WHAT && ($a cmp $b) == 0
}

proto prefix:<~>($) is pure { * }
multi prefix:<~>(\a)          { a.Stringy }

#?if parrot
proto infix:<~>($?, $?) is pure { * }
#?endif
#?if !parrot
proto infix:<~>(Mu $?, Mu $?) is pure { * }
#?endif
multi infix:<~>($x = '')       { $x.Stringy }
multi infix:<~>(\a, \b)      { a.Stringy ~ b.Stringy }

#?if parrot
proto infix:<x>($?, $?)        { * }
#?endif
#?if !parrot
proto infix:<x>(Mu $?, Mu $?)        { * }
#?endif
multi infix:<x>()              { fail "No zero-arg meaning for infix:<x>" }
multi infix:<x>($x)            { $x.Stringy }
multi infix:<x>($s, $n)        { $s.Stringy x ($n.Int // 0) }

#?if parrot
proto infix:<leg>($?, $?) is pure { * }
#?endif
#?if !parrot
proto infix:<leg>(Mu $?, Mu $?) is pure { * }
#?endif
multi infix:<leg>(\a, \b)      { a.Stringy cmp b.Stringy }

#?if parrot
proto infix:<eq>($?, $?)  is pure { * }
#?endif
#?if !parrot
proto infix:<eq>(Mu $?, Mu $?)  is pure { * }
#?endif
multi infix:<eq>($x?)          { Bool::True }
multi infix:<eq>(\a, \b)       { a.Stringy eq b.Stringy }

proto infix:<ne>(Mu $?, Mu $?) is pure { * }
multi infix:<ne>($x?)            { Bool::True }
multi infix:<ne>(Mu \a, Mu \b)   { a !eq b }

#?if parrot
proto infix:<lt>($?, $?) is pure { * }
#?endif
#?if !parrot
proto infix:<lt>(Mu $?, Mu $?) is pure { * }
#?endif
multi infix:<lt>($x?)          { Bool::True }
multi infix:<lt>(\a, \b)       { a.Stringy lt b.Stringy }

#?if parrot
proto infix:<le>($?, $?) is pure { * }
#?endif
#?if !parrot
proto infix:<le>(Mu $?, Mu $?) is pure { * }
#?endif
multi infix:<le>($x?)          { Bool::True }
multi infix:<le>(\a, \b)       { a.Stringy le b.Stringy }

#?if parrot
proto infix:<gt>($?, $?) is pure { * }
#?endif
#?if !parrot
proto infix:<gt>(Mu $?, Mu $?) is pure { * }
#?endif
multi infix:<gt>($x?)          { Bool::True }
multi infix:<gt>(\a, \b)       { a.Stringy gt b.Stringy }

#?if parrot
proto infix:<ge>($?, $?) is pure { * }
#?endif
#?if !parrot
proto infix:<ge>(Mu $?, Mu $?) is pure { * }
#?endif
multi infix:<ge>($x?)          { Bool::True }
multi infix:<ge>(\a, \b)       { a.Stringy ge b.Stringy }

#?if parrot
proto infix:<~|>($?, $?) is pure { * }
#?endif
#?if !parrot
proto infix:<~|>(Mu $?, Mu $?) is pure { * }
#?endif
multi infix:<~|>($x = '')      { $x.Stringy }
multi infix:<~|>(\a, \b)       { a.Stringy ~| b.Stringy }

#?if parrot
proto infix:<~^>($?, $?)  is pure { * }
#?endif
#?if !parrot
proto infix:<~^>(Mu $?, Mu $?)  is pure { * }
#?endif
multi infix:<~^>($x = '')      { $x.Stringy }
multi infix:<~^>(\a, \b)       { a.Stringy ~^ b.Stringy }

#?if parrot
proto infix:<~&>($?, $?) is pure { * }
#?endif
#?if !parrot
proto infix:<~&>(Mu $?, Mu $?) is pure { * }
#?endif
multi infix:<~&>()             { fail "No zero-arg meaning for infix:<~&>" }
multi infix:<~&>($x)           { $x.Stringy }
multi infix:<~&>(\a, \b)       { a.Stringy ~& b.Stringy }

#?if parrot
proto prefix:<~^>($) is pure { * }
#?endif
#?if !parrot
proto prefix:<~^>(Mu $) is pure { * }
#?endif
multi prefix:<~^>(\a)         { ~^ a.Stringy }

# vim: ft=perl6 expandtab sw=4
