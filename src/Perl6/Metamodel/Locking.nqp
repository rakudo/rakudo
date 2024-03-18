#- Metamodel::Locking  --------------------------------------------------------
# Base logic for locking sensitive parts of any metamodel class by providing
# a *single* lock per instance.
role Perl6::Metamodel::Locking {
    has $!locking;

    method TWEAK(*%_) { $!locking := NQPLock.new }

    method protect(&code) { $!locking.protect(&code) }
}

# vim: expandtab sw=4
