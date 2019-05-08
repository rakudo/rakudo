proto sub gethostname(*%) {*}
multi sub gethostname(--> Str:D){
    Rakudo::Deprecations.DEPRECATED('$*KERNEL.hostname()');
    $*KERNEL.hostname()
}

# vim: ft=perl6 expandtab sw=4
