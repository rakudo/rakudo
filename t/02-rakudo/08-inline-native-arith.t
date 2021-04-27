use v6;
use Test;
use nqp;
plan 4;

my $p = run($*EXECUTABLE, '--target=optimize',
    '-e', 'my int $i = 1; for ^10 { $i = $i * 2 }', :out);
like $p.out.slurp(:close), /mul_i/,
    '$i * 2 inlines to mul_i when $i is declared as int';

{ # https://colabti.org/irclogger/irclogger_log/perl6-dev?date=2018-04-21#l216
    ok nqp::p6trialbind(:($?, $?, *%), nqp::list(Int, int), nqp::list(33, 1)),
        'can trialbiand to a sig with slurpy named param';
}

{ # https://colabti.org/irclogger/irclogger_log/perl6-dev?date=2018-04-21#l366
    nok nqp::p6trialbind(:(| where *.so), nqp::list(), nqp::list()),
        'trial bind notices `where` in the capture';
    nok nqp::p6trialbind(:(*% where *.so), nqp::list(), nqp::list()),
        'trial bind notices `where` in the named slurpy';
}

# vim: expandtab shiftwidth=4
