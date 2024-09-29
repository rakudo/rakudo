role CompUnit::Repository::Installable does CompUnit::Repository {
    # Installs a distribution into the repository.
    method install(Distribution $dist) { ... }

    # Returns True if we can install modules (this will typically do a
    # .w check on the module database).
    method can-install(--> Bool:D) { ... }

    # Returns the Distribution objects for all installed distributions.
    method installed(--> Iterable:D) { }
}

# vim: expandtab shiftwidth=4
