Rakudo::Internals.REGISTER-DYNAMIC: '@*ARGS', {
    my @ARGS;
    my Mu $argiter := nqp::getcurhllsym('$!ARGITER');
    @ARGS.push(nqp::p6box_s(nqp::shift($argiter))) while $argiter;
    PROCESS::<@ARGS> := @ARGS;
}
Rakudo::Internals.REGISTER-DYNAMIC: '$*ARGFILES', {
    # Here, we use $*IN's attributes to init the arg files because
    # the $*ARGFILES won't get instantiated until first access and by that
    # time the user may have already modified $*IN's attributes to their liking
    PROCESS::<$ARGFILES> = @*ARGS
      ?? IO::ArgFiles.new(@*ARGS)
      !! IO::ArgFiles.new:
          (my $in := $*IN),
          :nl-in($in.nl-in), :chomp($in.chomp), :encoding($in.encoding),
          :bin(nqp::hllbool(nqp::isfalse($in.encoding)));
}

# vim: ft=perl6 expandtab sw=4
