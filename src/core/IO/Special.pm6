my class Instant { ... }

class IO::Special does IO {
    has Str $.what;

    multi method WHICH(IO::Special:D: --> ValueObjAt) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT, IO::Special),
              'IO::Special|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            $!what
          ),
          ValueObjAt
        )
    }

    method new(Str:D \what) {
        nqp::p6bindattrinvres(nqp::create(self),self,'$!what',what)
    }

    multi method Str (IO::Special:D: --> Str) {
        $!what
    }
    multi method gist(IO::Special:D: --> Str) {
        nqp::p6box_s(nqp::concat(nqp::decont_s($!what), '.IO'))
    }
    multi method perl(IO::Special:D: --> Str) {
        nqp::p6box_s(
          nqp::concat(
            nqp::concat(nqp::unbox_s(self.^name), '.new('),
            nqp::concat(nqp::unbox_s($!what.perl), ')')
          )
        )
    }

    multi method e       (IO::Special:D: --> True)    {         }
    multi method d       (IO::Special:D: --> False)   {         }
    multi method f       (IO::Special:D: --> False)   {         }
    multi method s       (IO::Special:D:--> 0)        {         }
    multi method l       (IO::Special:D: --> False)   {         }
    multi method r       (IO::Special:D: --> Bool)    {
        $!what eq '<STDIN>'
    }
    multi method w       (IO::Special:D: --> Bool)    {
        $!what eq '<STDOUT>' or $!what eq '<STDERR>'
    }
    multi method x       (IO::Special:D: --> False)   {         }
    multi method rw      (IO::Special:D: --> False)   {         }
    multi method rwx     (IO::Special:D: --> False)   {         }
    multi method modified(IO::Special:D: --> Instant) { Instant }
    multi method accessed(IO::Special:D: --> Instant) { Instant }
    multi method changed (IO::Special:D: --> Instant) { Instant }
    multi method mode    (IO::Special:D: --> IntStr)  { IntStr  }
}

# vim: ft=perl6 expandtab sw=4
