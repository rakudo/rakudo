my role Stringy { }

proto prefix:<~>(|$) { * }
multi prefix:<~>(\$a)          { $a.Stringy }

proto infix:<~>(|$)            { * }
multi infix:<~>($x = '')       { $x.Stringy }
multi infix:<~>(\$a, \$b)      { $a.Stringy ~ $b.Stringy }

proto infix:<x>(|$)            { * }
multi infix:<x>()              { fail "No zero-arg meaning for infix:<x>" }
multi infix:<x>($x)            { $x.Stringy }
multi infix:<x>($s, $n)        { $s.Stringy x $n.Numeric }

proto infix:<leg>(|$) { * }
multi infix:<leg>(\$a, \$b)    { $a.Stringy cmp $b.Stringy }

proto infix:<eq>(|$)           { * }
multi infix:<eq>($x?)          { Bool::True }
multi infix:<eq>(\$a, \$b)     { $a.Stringy eq $b.Stringy }

proto infix:<ne>(|$)             { * }
multi infix:<ne>($x?)            { Bool::False }
multi infix:<ne>(Mu \$a, Mu \$b) { $a !eq $b }

proto infix:<lt>(|$)           { * }
multi infix:<lt>($x?)          { Bool::True }
multi infix:<lt>(\$a, \$b)     { $a.Stringy lt $b.Stringy }

proto infix:<le>(|$)           { * }
multi infix:<le>($x?)          { Bool::True }
multi infix:<le>(\$a, \$b)     { $a.Stringy le $b.Stringy }

proto infix:<gt>(|$)           { * }
multi infix:<gt>($x?)          { Bool::True }
multi infix:<gt>(\$a, \$b)     { $a.Stringy gt $b.Stringy }

proto infix:<ge>(|$)           { * }
multi infix:<ge>($x?)          { Bool::True }
multi infix:<ge>(\$a, \$b)     { $a.Stringy ge $b.Stringy }

proto infix:<~|>(|$)           { * }
multi infix:<~|>($x = '')      { $x.Stringy }
multi infix:<~|>(\$a, \$b)     { $a.Stringy ~| $b.Stringy }

proto infix:<~^>(|$)           { * }
multi infix:<~^>($x = '')      { $x.Stringy }
multi infix:<~^>(\$a, \$b)     { $a.Stringy ~^ $b.Stringy }

proto infix:<~&>(|$)           { * }
multi infix:<~&>()             { fail "No zero-arg meaning for infix:<~&>" }
multi infix:<~&>($x)           { $x.Stringy }
multi infix:<~&>(\$a, \$b)     { $a.Stringy ~& $b.Stringy }

proto prefix:<~^>(|$)          { * }
multi prefix:<~^>(\$a)         { ~^ $a.Stringy }

