my class Method { # declared in BOOTSTRAP
    # class Method is Routine { ... }

    multi method gist(Method:D:) { self.name }
}

multi sub trait_mod:<is>(Method $m, :$cached!) {
    X::NYI.new(:feature("'is cached' on methods")).throw;
}

# vim: ft=perl6 expandtab sw=4
