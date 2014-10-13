class IO::Special {
    has Str $.what;

    submethod BUILD(:$!what!)    { }
    method WHICH(IO::Special:D:) { "IO::Special$!what" }
    method Str(IO::Special:D:)   { $!what }

    method IO(IO::Special:D:) { self }

    method e(IO::Special:D:) { True }
    method d(IO::Special:D:) { False }
    method f(IO::Special:D:) { False }
    method s(IO::Special:D:) { 0 }
    method l(IO::Special:D:) { False }
    method r(IO::Special:D:) { $!what eq '<IN>' }
    method w(IO::Special:D:) { $!what eq '<OUT>' or $!what eq '<ERR>' }
    method x(IO::Special:D:) { False }
    method modified(IO::Special:D:) { Instant }
    method accessed(IO::Special:D:) { Instant }
    method changed(IO::Special:D:)  { Instant}
}
