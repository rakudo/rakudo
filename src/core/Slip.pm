# A Slip is a kind of List that is immediately incorporated into an iteration
# or another List. Other than that, it's a totally normal List.
my class Slip { # is List
    multi method Slip(Slip:D:) { self }
    method defined ()          { self.so }
    method CALL-ME (+args)     { args.Slip }
    multi method perl(Slip:D:) {
        my $guts := callsame;
        nqp::if(
            nqp::eqat($guts, '$', 0), # we're itemized
            nqp::concat('$(slip', nqp::concat(nqp::substr($guts, 1), ')')),
            nqp::concat('slip', $guts),
        )
    }
}

# The slip(...) function creates a Slip.
proto slip(|)     { * }
multi slip()      { Empty }
multi slip(@args) { @args.Slip }
multi slip(+args) { args.Slip }

# vim: ft=perl6 expandtab sw=4
