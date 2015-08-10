# A Slip is a kind of List that is immediately incorporated into an iteration
# or another List. Other than that, it's a totally normal List.
my class Slip is List {
    multi method Slip(Slip:D:) { self }
}

# vim: ft=perl6 expandtab sw=4
