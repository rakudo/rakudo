class IO::Special {
    has Str $.what;

    submethod BUILD(:$!what!) { }

    method IO { self }

    method e() { True }
    method d() { False }
    method f() { False }
    method s() { 0 }
    method l() { False }
    method r() { $!what eq '<IN>' }
    method w() { $!what eq '<OUT>' or $!what eq '<ERR>' }
    method x() { False }
    method modified() { Instant }
    method accessed() { Instant }
    method changed()  { Instant}
}
