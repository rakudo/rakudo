Rakudo::Internals.REGISTER-DYNAMIC: "6.d\0" ~ '$*ARGFILES', {
    PROCESS::<$ARGFILES> = @*ARGS ?? IO::CatHandle.new(@*ARGS).open !! $*IN;
}

# vim: ft=perl6 expandtab sw=4
