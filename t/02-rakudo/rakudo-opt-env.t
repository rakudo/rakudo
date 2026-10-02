use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 7;

my $optimize-level = 'use nqp; print nqp::atkey(nqp::getcomp("Raku").cli-options, "optimize")';

{
    temp %*ENV<RAKUDO_OPT> = '--optimize=off';
    is-run $optimize-level, :out<off>,
      'an option from RAKUDO_OPT reaches the compiler';
    is-run $optimize-level, :compiler-args['--optimize=3'], :out<3>,
      'the same option on the command line wins over RAKUDO_OPT';
    is-run $optimize-level, :compiler-args['--encoding', 'utf8', '--optimize=3'], :out<3>,
      'the command line option still wins after an option with a separate value';
    is-run $optimize-level, :args['--optimize=3'], :out<off>,
      'an argument to the program after -e does not count as the option';
    is-run 'print 1', :compiler-args['-', '--optimize=3'], :in($optimize-level), :out<off>,
      'an argument after a program read from standard input does not count';
}
{
    temp %*ENV<RAKUDO_OPT> = '--ll-exception';
    is-run 'use nqp; print nqp::islist(nqp::atkey(nqp::getcomp("Raku").cli-options, "ll-exception"))',
      :compiler-args['--ll-exception'], :out<0>,
      'a flag given both ways is passed on once';
}
{
    temp %*ENV<RAKUDO_OPT> = '-Ifrom-env';
    is-run 'use nqp; print nqp::elems(nqp::atkey(nqp::getcomp("Raku").cli-options, "I"))',
      :compiler-args['-Ifrom-command-line'], :out<2>,
      'an include path from RAKUDO_OPT accumulates with one from the command line';
}

# vim: expandtab shiftwidth=4
