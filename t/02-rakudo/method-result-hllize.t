use Test;
use nqp;

plan 5;

my $c = q[use nqp; my class C { method m { nqp::list(1, 2) }; method n { 'str' } }; ];

is EVAL($c ~ q[(C . m).^name]), 'List',
    'the dotty infix maps a VM array result to a List';

is EVAL($c ~ q[my $x = C; $x .= m; $x.^name]), 'List',
    '.= on a variable maps a VM array result to a List';

is EVAL($c ~ q[my @a = C; @a[0] .= m; @a[0].^name]), 'List',
    '.= on an element maps a VM array result to a List';

is EVAL($c ~ q[my $x = C; given $x { .=m }; $x.^name]), 'List',
    '.= on the topic maps a VM array result to a List';

is EVAL($c ~ q[my $x is default(C) .= m; $x.^name]), 'List',
    'a .= initializer of a variable maps a VM array result to a List';

# vim: expandtab shiftwidth=4
