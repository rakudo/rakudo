# XXX role Numeric { ... }
my class Numeric {
    method Numeric() { self }

    multi method ACCEPTS(Numeric:D: $a) { $a == self }

    proto method log(|$) {*}
}


## arithmetic operators

proto prefix:<+>(|$) { * }
multi prefix:<+>(\$a) { $a.Numeric }
multi prefix:<+>(Numeric \$a) { $a }

proto prefix:<->(|$) { * }

proto prefix:<abs>(|$) { * }
multi prefix:<abs>(\$a)      { abs $a.Numeric }

proto sub log(|$) {*}
multi sub log(Numeric $x) { $x.log }
multi sub log(Numeric $x, Numeric $base) { $x.log($base) }

proto infix:<+>(|$) { * }
multi infix:<+>(\$a, \$b)    { $a.Numeric + $b.Numeric }

proto infix:<->(|$) { * }
multi infix:<->(\$a, \$b)    { $a.Numeric - $b.Numeric }

proto infix:<*>(|$) { * }
multi infix:<*>(\$a, \$b)    { $a.Numeric * $b.Numeric }

proto infix:</>(|$) { * }
multi infix:</>(\$a, \$b)    { $a.Numeric / $b.Numeric }

proto infix:<div>(|$) { * }

proto infix:<%>(|$) { * }
multi infix:<%>(\$a, \$b)    { $a.Numeric % $b.Numeric }

proto infix:<lcm>(|$) { * }
multi infix:<lcm>(\$a, \$b) { $a.Numeric lcm $b.Numeric }

proto infix:<gcd>(|$) { * }
multi infix:<gcd>(\$a, \$b) { $a.Numeric gcd $b.Numeric }

proto infix:<**>(|$) { * }
multi infix:<**>(\$a, \$b) { $a.Numeric ** $b.Numeric }



## relational operators

proto infix:«<=>»(|$) { * }
multi infix:«<=>»(\$a, \$b)   { $a.Numeric <=> $b.Numeric }

proto infix:<==>(|$) { * }
multi infix:<==>(\$a, \$b)   { $a.Numeric == $b.Numeric }

proto infix:<!=>(|$) { * }
multi infix:<!=>(\$a, \$b)   { $a.Numeric != $b.Numeric }

proto infix:«<»(|$) { * }
multi infix:«<»(\$a, \$b)    { $a.Numeric < $b.Numeric }

proto infix:«<=»(|$) { * }
multi infix:«<=»(\$a, \$b)   { $a.Numeric <= $b.Numeric }

proto infix:«>»(|$) { * }
multi infix:«>»(\$a, \$b)    { $a.Numeric > $b.Numeric }

proto infix:«>=»(|$) { * }
multi infix:«>=»(\$a, \$b)   { $a.Numeric >= $b.Numeric }



## bitwise operators

proto infix:<+|>(|$) { * }

proto infix:<+&>(|$) { * }

proto infix:<+^>(|$) { * }

proto infix:«+<»(|$) { * }

proto infix:«+>»(|$) { * }

proto prefix:<+^>(|$) { * }


