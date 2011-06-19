# XXX role Numeric { ... }
my class Numeric {
    method Numeric() { self }

    multi method ACCEPTS(Numeric:D: $a) { $a == self }
}


## arithmetic operators

proto prefix:<+>(|$) { * }
multi prefix:<+>(\$a) { $a.Numeric }
multi prefix:<+>(Numeric \$a) { $a }

proto prefix:<->(|$) { * }

proto prefix:<abs>(|$) { * }
multi prefix:<abs>(\$a)      { abs $a.Numeric }


proto infix:<+>(|$) { * }
multi infix:<+>(\$a, \$b)    { $a.Numeric + $b.Numeric }

proto infix:<->(|$) { * }
multi infix:<->(\$a, \$b)    { $a.Numeric + $b.Numeric }

proto infix:<*>(|$) { * }
multi infix:<*>(\$a, \$b)    { $a.Numeric * $b.Numeric }

proto infix:</>(|$) { * }
multi infix:</>(\$a, \$b)    { $a.Numeric / $b.Numeric }

proto infix:<div>(|$) { * }

proto infix:<%>(|$) { * }
multi infix:<%>(\$a, \$b)    { $a.Numeric % $b.Numeric }

proto infix:<**>(|$) { * }
multi infix:<**>(\$a, \$b) { $a.Numeric ** $b.Numeric }

proto infix:<lcm>(|$) { * }

proto infix:<gcd>(|$) { * }



## relational operators

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


