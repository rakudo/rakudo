# API to obtain the data of any addressable content
role Distribution {
    # `meta` provides an API to the meta data in META6 spec (s22)
    #   -   A Distribution may be represented internally by some other
    #       spec (such as using the file system itself for prereqs), as
    #       long as it can also be represented as the META6 hash format
    method meta(--> Hash) { ... }

    # `content($content-id)` provides an API to the data itself
    #   -   Use `.meta` to determine the $address of a specific $content-id
    #   -   IO::Handle is meant to be a data stream that may or may not be available; for now
    #       it would return an IO::Handle and have `.open(:bin).slurp` called on it. So if
    #       a socket wants to handle this role currently it would have to wrap `open` or `.slurp`
    #       to handle any protocol negotiation as well as probably saving the data to a tmpfile and
    #       return an IO::Handle to that

    method content($content-id --> IO::Handle) { ... }
}

# vim: expandtab shiftwidth=4
