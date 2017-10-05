my class Instant { ... }

class IO::Special does IO {
    has Str $.what;

    method new(\what) {
        nqp::p6bindattrinvres(nqp::create(self),self,'$!what',what)
    }
    multi method WHICH(IO::Special:D:) {        "IO::Special$!what"        }
    multi method Str  (IO::Special:D:) {                    $!what         }
    multi method perl (IO::Special:D:) { "{self.^name}.new({$!what.perl})" }

    method IO(IO::Special:D:) { self }

    method e(IO::Special:D: --> True) { }
    method d(IO::Special:D: --> False) { }
    method f(IO::Special:D: --> False) { }
    method s(IO::Special:D:--> 0) { }
    method l(IO::Special:D: --> False) { }
    method r(IO::Special:D:) { $!what eq '<STDIN>' }
    method w(IO::Special:D:) { $!what eq '<STDOUT>' or $!what eq '<STDERR>' }
    method x(IO::Special:D: --> False) { }
    method modified(IO::Special:D: --> Instant) { Instant }
    method accessed(IO::Special:D: --> Instant) { Instant }
    method changed( IO::Special:D: --> Instant) { Instant }
    method mode(IO::Special:D: --> Nil) { }
}
