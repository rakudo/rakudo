my role Stringy { }

multi sub infix:<eqv>(Stringy:D $a, Stringy:D $b) {
    $a.WHAT === $b.WHAT && ($a cmp $b) == 0
}

proto prefix:<~>($) is pure { * }
multi prefix:<~>(\a)          { a.Stringy }

proto infix:<~>($?, $?) is pure { * }
multi infix:<~>($x = '')       { $x.Stringy }
multi infix:<~>(\a, \b)      { a.Stringy ~ b.Stringy }

proto infix:<x>($?, $?)        { * }
multi infix:<x>()              { fail "No zero-arg meaning for infix:<x>" }
multi infix:<x>($x)            { $x.Stringy }
multi infix:<x>($s, $n)        { $s.Stringy x ($n.Int // 0) }

proto infix:<leg>($?, $?) is pure { * }
multi infix:<leg>(\a, \b)      { a.Stringy cmp b.Stringy }

proto infix:<eq>($?, $?)  is pure { * }
multi infix:<eq>($x?)          { Bool::True }
multi infix:<eq>(\a, \b)       { a.Stringy eq b.Stringy }

proto infix:<ne>(Mu $?, Mu $?) is pure { * }
multi infix:<ne>($x?)            { Bool::True }
multi infix:<ne>(Mu \a, Mu \b)   { a !eq b }

proto infix:<lt>($?, $?) is pure { * }
multi infix:<lt>($x?)          { Bool::True }
multi infix:<lt>(\a, \b)       { a.Stringy lt b.Stringy }

proto infix:<le>($?, $?) is pure { * }
multi infix:<le>($x?)          { Bool::True }
multi infix:<le>(\a, \b)       { a.Stringy le b.Stringy }

proto infix:<gt>($?, $?) is pure { * }
multi infix:<gt>($x?)          { Bool::True }
multi infix:<gt>(\a, \b)       { a.Stringy gt b.Stringy }

proto infix:<ge>($?, $?) is pure { * }
multi infix:<ge>($x?)          { Bool::True }
multi infix:<ge>(\a, \b)       { a.Stringy ge b.Stringy }

proto infix:<~|>($?, $?) is pure { * }
multi infix:<~|>($x = '')      { $x.Stringy }
multi infix:<~|>(\a, \b)       { a.Stringy ~| b.Stringy }

proto infix:<~^>($?, $?)  is pure { * }
multi infix:<~^>($x = '')      { $x.Stringy }
multi infix:<~^>(\a, \b)       { a.Stringy ~^ b.Stringy }

proto infix:<~&>($?, $?) is pure { * }
multi infix:<~&>()             { fail "No zero-arg meaning for infix:<~&>" }
multi infix:<~&>($x)           { $x.Stringy }
multi infix:<~&>(\a, \b)       { a.Stringy ~& b.Stringy }

proto prefix:<~^>($?, $?) is pure { * }
multi prefix:<~^>(\a)         { ~^ a.Stringy }

