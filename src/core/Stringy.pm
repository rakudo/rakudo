# role Stringy { ... }

proto prefix:<~>(|$) { * }
multi prefix:<~>(\$a)          { $a.Stringy }

proto infix:<~>(|$) { * }
multi infix:<~>(\$a, \$b) { $a.Stringy ~ $b.Stringy }

proto infix:<eq>(|$) { * }
multi infix:<eq>(\$a, \$b)     { $a.Stringy eq $b.Stringy }

proto infix:<ne>(|$) { * }
multi infix:<ne>(\$a, \$b)     { $a.Stringy ne $b.Stringy }

proto infix:<lt>(|$) { * }
multi infix:<lt>(\$a, \$b)     { $b.Stringy lt $b.Stringy }

proto infix:<le>(|$) { * }
multi infix:<le>(\$a, \$b)     { $b.Stringy le $b.Stringy }

proto infix:<gt>(|$) { * }
multi infix:<gt>(\$a, \$b)     { $b.Stringy gt $b.Stringy }

proto infix:<ge>(|$) { * }
multi infix:<ge>(\$a, \$b)     { $b.Stringy ge $b.Stringy }

proto infix:<leg>(|$) { * }
multi infix:<leg>(\$a, \$b)    { $b.Stringy cmp $b.Stringy }

proto infix:<~|>(|$) { * }
multi infix:<~|>(\$a, \$b)     { $b.Stringy ~| $b.Stringy }

proto infix:<~&>(|$) { * }
multi infix:<~&>(\$a, \$b)     { $b.Stringy ~& $b.Stringy }

proto infix:<~^>(|$) { * }
multi infix:<~^>(\$a, \$b)     { $b.Stringy ~^ $b.Stringy }

proto prefix:<~^>(|$) { * }
multi prefix:<~^>(\$a, \$b)    { ~^ $b.Stringy }


