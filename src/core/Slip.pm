# A Slip is a kind of List that is immediately incorporated into an iteration
# or another List. Other than that, it's a totally normal List.
my class Slip {
    multi method Slip(Slip:D:) { self }
    method defined () { self.so }
    method CALL-ME (+args) { args.Slip }
}

# The slip(...) function creates a Slip.
proto slip(|) { * }
multi slip() { Empty }
multi slip(+args) { args.Slip }

# vim: ft=perl6 expandtab sw=4
