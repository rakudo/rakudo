proto sub gethostname(*%) is implementation-detail {*}
multi sub gethostname(--> Str:D){
    Rakudo::Deprecations.DEPRECATED('$*KERNEL.hostname()');
    $*KERNEL.hostname()
}

# vim: expandtab shiftwidth=4
