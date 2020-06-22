# enum Bool declared in BOOTSTRAP
BEGIN {
    Bool.^add_method('Bool',    my proto method Bool(|)    {*});
    Bool.^add_method('gist',    my proto method gist(|)    {*});
    Bool.^add_method('Numeric', my proto method Numeric(|) {*});
    Bool.^add_method('Int',     my proto method Int(|)     {*});
    Bool.^add_method('ACCEPTS', my proto method ACCEPTS(|) {*});
    Bool.^add_method('pick',    my proto method pick(|) {*});
    Bool.^add_method('roll',    my proto method roll(|) {*});
    Bool.^add_method('raku',    my proto method raku(|) {*});
}
BEGIN {
    Bool.^add_multi_method('Bool',    my multi method Bool(Bool:D:)    { self });
    Bool.^add_multi_method('gist',    my multi method gist(Bool:D:)    { self ?? 'True' !! 'False' });
    Bool.^add_multi_method('Str',     my multi method Str(Bool:D:)     { self ?? 'True' !! 'False' });
    Bool.^add_multi_method('Numeric', my multi method Numeric(Bool:D:) { self ?? 1 !! 0 });
    Bool.^add_multi_method('Int',     my multi method Int(Bool:D:)     { self ?? 1 !! 0 });
    Bool.^add_multi_method('Real',    my multi method Real(Bool:D:)    { self ?? 1 !! 0 });
    Bool.^add_multi_method('ACCEPTS', my multi method ACCEPTS(Bool:D: Mu \topic ) { self });
    Bool.^add_multi_method('raku', my multi method raku(Bool:D:) { self ?? 'Bool::True' !! 'Bool::False' });

    Bool.^add_multi_method('pick', my multi method pick(Bool:U:)    { nqp::hllbool(nqp::isge_n(nqp::rand_n(2e0), 1e0)) });
    Bool.^add_multi_method('roll', my multi method roll(Bool:U:)    { nqp::hllbool(nqp::isge_n(nqp::rand_n(2e0), 1e0)) });
}
BEGIN {
    Bool.^add_multi_method('Bool',    my multi method Bool(Bool:U:)    { Bool::False });
    Bool.^add_multi_method('ACCEPTS', my multi method ACCEPTS(Bool:U: \topic ) { nqp::istype(topic, Bool) });
    Bool.^add_multi_method('gist',    my multi method gist(Bool:U:)    { '(Bool)' });
    Bool.^add_multi_method('raku', my multi method raku(Bool:U:) { 'Bool' });

    Bool.^add_multi_method('pick', my multi method pick(Bool:U: $n) { self.^enum_value_list.pick($n) });
    Bool.^add_multi_method('roll', my multi method roll(Bool:U: $n) { self.^enum_value_list.roll($n) });

    Bool.^add_method('pred',  my method pred() { Bool::False });
    Bool.^add_method('succ',  my method succ() { Bool::True });

    Bool.^add_method('enums', my method enums() { self.^enum_values.Map });

    Bool.^compose;
}

multi sub prefix:<++>(Bool $a is rw)  { $a = True; }
multi sub prefix:<-->(Bool $a is rw)  { $a = False; }
multi sub postfix:<++>(Bool:U $a is rw --> False) { $a = True }
multi sub postfix:<-->(Bool:U $a is rw) { $a = False; }

multi sub postfix:<++>(Bool:D $a is rw) {
    if $a {
        True
    }
    else {
        $a = True;
        False
    }
}
multi sub postfix:<-->(Bool:D $a is rw) {
    if $a {
        $a = False;
        True
    }
    else {
        False
    }
}

proto sub prefix:<?>(Mu, *%) is pure {*}
multi sub prefix:<?>(Bool:D \a) { a }
multi sub prefix:<?>(Bool:U \a) { Bool::False }
multi sub prefix:<?>(Mu \a) { a.Bool }

proto sub prefix:<so>(Mu, *%) is pure {*}
multi sub prefix:<so>(Bool:D \a) { a }
multi sub prefix:<so>(Bool:U \a) { Bool::False }
multi sub prefix:<so>(Mu \a) { a.Bool }

proto sub prefix:<!>(Mu, *%) is pure {*}
multi sub prefix:<!>(Bool \a) { nqp::hllbool(nqp::not_i(nqp::istrue(a))) }
multi sub prefix:<!>(Mu \a) { nqp::hllbool(nqp::not_i(nqp::istrue(a))) }
multi sub prefix:<!>(Mu \a, :$exists!) {
    die "Precedence issue with ! and :exists, perhaps you meant :!exists?"
}

proto sub prefix:<not>(Mu, *%) is pure {*}
multi sub prefix:<not>(Bool \a) { nqp::hllbool(nqp::not_i(nqp::istrue(a))) }
multi sub prefix:<not>(Mu \a) { nqp::hllbool(nqp::not_i(nqp::istrue(a))) }

proto sub prefix:<?^>(Mu, *%) is pure {*}
multi sub prefix:<?^>(Mu \a) { not a }

proto sub infix:<?&>(Mu $?, Mu $?, *%) is pure {*}
multi sub infix:<?&>(Mu $x = Bool::True) { $x.Bool }
multi sub infix:<?&>(Mu \a, Mu \b)       { a.Bool && b.Bool }

proto sub infix:<?|>(Mu $?, Mu $?, *%) is pure {*}
multi sub infix:<?|>(Mu $x = Bool::False) { $x.Bool }
multi sub infix:<?|>(Mu \a, Mu \b)        { a.Bool || b.Bool }

proto sub infix:<?^>(Mu $?, Mu $?, *%) is pure {*}
multi sub infix:<?^>(Mu $x = Bool::False) { $x.Bool }
multi sub infix:<?^>(Mu \a, Mu \b)        { nqp::hllbool(nqp::ifnull(nqp::xor(a.Bool,b.Bool), 0)) }

# These operators are normally handled as macros in the compiler;
# we define them here for use as arguments to functions.
proto sub infix:<&&>(Mu $?, Mu $?, *%)    {*}
multi sub infix:<&&>(Mu $x = Bool::True)  { $x }
multi sub infix:<&&>(Mu \a, &b)           { a && b() }
multi sub infix:<&&>(Mu \a, Mu \b)        { a && b }

proto sub infix:<||>(Mu $?, Mu $?, *%)    {*}
multi sub infix:<||>(Mu $x = Bool::False) { $x }
multi sub infix:<||>(Mu \a, &b)           { a || b() }
multi sub infix:<||>(Mu \a, Mu \b)        { a || b }

proto sub infix:<^^>(|)                   {*}
multi sub infix:<^^>(Mu $x = Bool::False) { $x }
multi sub infix:<^^>(Mu \a, &b)           { a ^^ b() }
multi sub infix:<^^>(Mu \a, Mu \b)        { a ^^ b }
multi sub infix:<^^>(+@a) {
    my Mu $a = shift @a;
    while @a {
        my Mu $b := shift @a;
        $b := $b() if $b ~~ Callable;
        next unless $b;
        return Nil if $a;
        $a := $b;
    }
    $a;
}

proto sub infix:<//>(Mu $?, Mu $?, *%)    {*}
multi sub infix:<//>(Mu $x = Any)         { $x }
multi sub infix:<//>(Mu \a, &b)           { a // b }
multi sub infix:<//>(Mu \a, Mu \b)        { a // b }

proto sub infix:<and>(Mu $?, Mu $?, *%)   {*}
multi sub infix:<and>(Mu $x = Bool::True) { $x }
multi sub infix:<and>(Mu \a, &b)          { a && b }
multi sub infix:<and>(Mu \a, Mu \b)       { a && b }

proto sub infix:<or>(Mu $?, Mu $?, *%)    {*}
multi sub infix:<or>(Mu $x = Bool::False) { $x }
multi sub infix:<or>(Mu \a, &b)           { a || b }
multi sub infix:<or>(Mu \a, Mu \b)        { a || b }

proto sub infix:<xor>(|)   {*}
multi sub infix:<xor>(Mu $x = Bool::False) { $x }
multi sub infix:<xor>(Mu \a, &b)          { a ^^ b }
multi sub infix:<xor>(Mu \a, Mu \b)       { a ^^ b }
multi sub infix:<xor>(|c)                 { &infix:<^^>(|c); }

# vim: expandtab shiftwidth=4
