use v6.e.PREVIEW;
use Test;

plan 5;

{
    is-deeply (L: for ^5 { next 42 if $_ == 1; last L if $_ == 3; $_ }).List, (0, 42, 2),
        'a next with a value gives that value in a loop where a labeled last gives none';
}
{
    is-deeply (L: for ^5 { last 42 if $_ == 2; next L if $_ == 0; $_ }).List, (1, 42),
        'a last with a value gives that value in a loop where a labeled next gives none';
}
{
    my $i = 0;
    is-deeply (L: while $i < 4 { next 42 if ++$i == 1; last L if $i == 3; $i }).List, (42, 2),
        'a next with a value gives that value in a while loop where a labeled last gives none';
}
{
    is-deeply (L: for ^9 -> $a, $b, $c { NEXT { }; next 42 if $a == 3; next L if $a == 6; $a }).List,
        (0, 42),
        'a next with a value gives that value in a loop taking three values with a phaser where a labeled next gives none';
}
{
    is-deeply (L: hyper for ^5 { next 42 if $_ == 1; next L if $_ == 3; $_ }).List, (0, 42, 2, 4),
        'a next with a value gives that value in a hyper for loop where a labeled next gives none';
}

# vim: expandtab shiftwidth=4
