role CompUnit::Repository::Installable does CompUnit::Repository {
    # Installs a distribution into the repository.
    method install(
        # A Distribution object 
        Distribution $dist,
        # A hash mapping entries in `provides` to a disk location that
        # holds the source files; they will be copied (and may also be
        # precompiled by some CompUnit::Repository implementations).
        %sources,
        # A hash mapping entries in the `resources` to a disk location
        # that holds the files; again, these will be copied and stored.
        %resources)
        { ... }

    # Returns True if we can install modules (this will typically do a
    # .w check on the module database).
    method can-install() returns Bool { ... }

    # Returns the Distribution objects for all installed distributions.
    method installed() returns Iterable { }
}

# vim: ft=perl6 expandtab sw=4
