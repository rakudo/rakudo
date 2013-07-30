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

    method pick(Bool:U: $n = 1) { (Bool::True, Bool::False).pick($n) }
    method roll(Bool:U: $n = 1) { (Bool::True, Bool::False).roll($n) }

    multi method ACCEPTS(Bool:D: Mu \topic ) { self }

    multi method perl(Bool:D:) { self ?? 'Bool::True' !! 'Bool::False' }

    method enums() {
        my % = False => 0, True => 1
    }
}

multi prefix:<++>(Bool:U \a is rw)  { a = True; }
multi prefix:<-->(Bool:U \a is rw)  { a = False; }
multi postfix:<++>(Bool:U \a is rw) { a = True; False; }
multi postfix:<-->(Bool:U \a is rw) { a = False; }

proto prefix:<?>(Mu $) is pure { * }
multi prefix:<?>(Bool:D \a) { a }
multi prefix:<?>(Mu \a) { a.Bool }

proto prefix:<so>(Mu $) is pure { * }
multi prefix:<so>(Bool:D \a) { a }
multi prefix:<so>(Mu \a) { a.Bool }

proto prefix:<!>(Mu $) is pure { * }
multi prefix:<!>(Bool \a) { nqp::p6bool(a ?? 0 !! 1) }
multi prefix:<!>(Mu \a) { nqp::p6bool(a.Bool ?? 0 !! 1) }

proto prefix:<not>(Mu $) is pure { * }
multi prefix:<not>(Bool \a) { nqp::p6bool(a ?? 0 !! 1) }
multi prefix:<not>(Mu \a) { nqp::p6bool(a.Bool ?? 0 !! 1) }

proto prefix:<?^>(Mu $) is pure { * }
multi prefix:<?^>(Mu \a) { not a }

proto infix:<?&>(|) is pure          { * }
multi infix:<?&>(Mu $x = Bool::True) { $x.Bool }
multi infix:<?&>(Mu \a, Mu \b)       { a.Bool && b.Bool }

proto infix:<?|>(|) is pure           { * }
multi infix:<?|>(Mu $x = Bool::False) { $x.Bool }
multi infix:<?|>(Mu \a, Mu \b)        { a.Bool || b.Bool }

proto infix:<?^>(|) is pure           { * }
multi infix:<?^>(Mu $x = Bool::False) { $x.Bool }
multi infix:<?^>(Mu \a, Mu \b)        { nqp::p6bool(nqp::ifnull(nqp::xor(a.Bool,b.Bool), 0)) }

# These operators are normally handled as macros in the compiler;
# we define them here for use as arguments to functions.
proto infix:<&&>(|)                   { * }
multi infix:<&&>(Mu $x = Bool::True)  { $x }
multi infix:<&&>(Mu \a, Mu \b)        { a && b }

proto infix:<||>(|)                   { * }
multi infix:<||>(Mu $x = Bool::False) { $x }
multi infix:<||>(Mu \a, Mu \b)        { a || b }

proto infix:<^^>(|)                   { * }
multi infix:<^^>(Mu $x = Bool::False) { $x }
multi infix:<^^>(Mu \a, Mu \b)        { a ^^ b }
multi infix:<^^>(*@a) {
    my $a = shift @a;
    while @a {
        my $b := shift @a;
        next unless $b;
        return Nil if $a;
        $a := $b;
    }
    $a;
}

proto infix:<//>(|)                   { * }
multi infix:<//>(Mu $x = Any)         { $x }
multi infix:<//>(Mu \a, Mu \b)        { a // b }

proto infix:<and>(|)                  { * }
multi infix:<and>(Mu $x = Bool::True) { $x }
multi infix:<and>(Mu \a, Mu \b)       { a && b }

proto infix:<or>(|)                   { * }
multi infix:<or>(Mu $x = Bool::False) { $x }
multi infix:<or>(Mu \a, Mu \b)        { a || b }

proto infix:<xor>(|)                   { * }
multi infix:<xor>(Mu $x = Bool::False) { $x }
multi infix:<xor>(Mu \a, Mu \b)        { a ^^ b }
multi infix:<xor>(*@a)                 { &infix:<^^>(@a); }

proto infix:<orelse>(|)               { * }
multi infix:<orelse>(Mu $x = Any)     { $x }
multi infix:<orelse>(Mu \a, Mu \b)    { a // b }
