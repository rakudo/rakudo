my class IO::ArgFiles is IO::CatHandle {

    # This class exists for backwards compatibility reasons.
    # There used to be no IO::CatHandle and IO::ArgFiles did the $*ARGFILES.
    # Now all the functionality has been subsumed by IO::CatHandle and
    # we keep $*ARGFILES as IO::ArgFiles that is just an empty subclass
    # of IO::CatHandle type

}

# vim: expandtab shiftwidth=4
