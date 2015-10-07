my class Bool { # declared in BOOTSTRAP
    # class Bool is Cool {
    #     has int $!value;

    multi method Bool(Bool:D:)    { self }
    multi method Numeric(Bool:D:) { self ?? 1 !! 0 }
    multi method Str(Bool:D:)     { self ?? 'True' !! 'False' }
    multi method gist(Bool:D:)    { self ?? 'True' !! 'False' }
    multi method DUMP(Bool:D:)    { self.Str }

    method Int()     { self ?? 1 !! 0 }

    method pred() { Bool::False }
    method succ() { Bool::True }

    method key() { self.Str }
    method value() { self.Numeric }

    proto method pick(|) { * }
    multi method pick(Bool:U:)    { nqp::p6bool(nqp::isge_n(nqp::rand_n(2e0), 1e0)) }
    multi method pick(Bool:U: $n) { (Bool::True,Bool::False).pick($n) }

    proto method roll(|) { * }
    multi method roll(Bool:U:)    { nqp::p6bool(nqp::isge_n(nqp::rand_n(2e0), 1e0)) }
    multi method roll(Bool:U: $n) { (Bool::True,Bool::False).roll($n) }

    multi method ACCEPTS(Bool:D: Mu \topic ) { self }

    multi method perl(Bool:D:) { self ?? 'Bool::True' !! 'Bool::False' }

    method enums() {
        my % = False => 0, True => 1
    }
}

multi sub prefix:<++>(Bool:U $a is rw)  { $a = True; }
multi sub prefix:<-->(Bool:U $a is rw)  { $a = False; }
multi sub postfix:<++>(Bool:U $a is rw) { $a = True; False; }
multi sub postfix:<-->(Bool:U $a is rw) { $a = False; }

proto sub prefix:<?>(Mu $) is pure { * }
multi sub prefix:<?>(Bool:D \a) { a }
multi sub prefix:<?>(Mu \a) { a.Bool }

proto sub prefix:<so>(Mu $) is pure { * }
multi sub prefix:<so>(Bool:D \a) { a }
multi sub prefix:<so>(Mu \a) { a.Bool }

proto sub prefix:<!>(Mu $) is pure { * }
multi sub prefix:<!>(Bool \a) { nqp::p6bool(nqp::not_i(nqp::istrue(a))) }
multi sub prefix:<!>(Mu \a) { nqp::p6bool(nqp::not_i(nqp::istrue(a))) }

proto sub prefix:<not>(Mu $) is pure { * }
multi sub prefix:<not>(Bool \a) { nqp::p6bool(nqp::not_i(nqp::istrue(a))) }
multi sub prefix:<not>(Mu \a) { nqp::p6bool(nqp::not_i(nqp::istrue(a))) }

proto sub prefix:<?^>(Mu $) is pure { * }
multi sub prefix:<?^>(Mu \a) { not a }

proto sub infix:<?&>(Mu $?, Mu $?) is pure { * }
multi sub infix:<?&>(Mu $x = Bool::True) { $x.Bool }
multi sub infix:<?&>(Mu \a, Mu \b)       { a.Bool && b.Bool }

proto sub infix:<?|>(Mu $?, Mu $?) is pure { * }
multi sub infix:<?|>(Mu $x = Bool::False) { $x.Bool }
multi sub infix:<?|>(Mu \a, Mu \b)        { a.Bool || b.Bool }

proto sub infix:<?^>(Mu $?, Mu $?) is pure { * }
multi sub infix:<?^>(Mu $x = Bool::False) { $x.Bool }
multi sub infix:<?^>(Mu \a, Mu \b)        { nqp::p6bool(nqp::ifnull(nqp::xor(a.Bool,b.Bool), 0)) }

# These operators are normally handled as macros in the compiler;
# we define them here for use as arguments to functions.
proto sub infix:<&&>(|)                   { * }
multi sub infix:<&&>(Mu $x = Bool::True)  { $x }
multi sub infix:<&&>(Mu \a, Mu \b)        { a && b }

proto sub infix:<||>(|)                   { * }
multi sub infix:<||>(Mu $x = Bool::False) { $x }
multi sub infix:<||>(Mu \a, Mu \b)        { a || b }

proto sub infix:<^^>(|)                   { * }
multi sub infix:<^^>(Mu $x = Bool::False) { $x }
multi sub infix:<^^>(Mu \a, Mu \b)        { a ^^ b }
multi sub infix:<^^>(+@a) {
    my $a = shift @a;
    while @a {
        my $b := shift @a;
        next unless $b;
        return Nil if $a;
        $a := $b;
    }
    $a;
}

proto sub infix:<//>(|)                   { * }
multi sub infix:<//>(Mu $x = Any)         { $x }
multi sub infix:<//>(Mu \a, Mu \b)        { a // b }

proto sub infix:<and>(|)                  { * }
multi sub infix:<and>(Mu $x = Bool::True) { $x }
multi sub infix:<and>(Mu \a, Mu \b)       { a && b }

proto sub infix:<or>(|)                   { * }
multi sub infix:<or>(Mu $x = Bool::False) { $x }
multi sub infix:<or>(Mu \a, Mu \b)        { a || b }

proto sub infix:<xor>(|)                  { * }
multi sub infix:<xor>(Mu $x = Bool::False) { $x }
multi sub infix:<xor>(Mu \a, Mu \b)        { a ^^ b }
multi sub infix:<xor>(|c)                 { &infix:<^^>(|c); }

# vim: ft=perl6 expandtab sw=4
