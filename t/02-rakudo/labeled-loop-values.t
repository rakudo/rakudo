use Test;

plan 22;

{
    is-deeply (L: for ^5 { next L if $_ == 2; $_ }).List, (0, 1, 3, 4),
        'a labeled next in a for loop gives no value';
}
{
    is-deeply (L: for ^5 { last L if $_ == 3; $_ }).List, (0, 1, 2),
        'a labeled last in a for loop gives no value';
}
{
    is-deeply (L: for ^5 { next L if $_ == 2; $_ }).head(4).List, (0, 1, 3, 4),
        'a labeled next in a for loop gives no value when values are taken one at a time';
}
{
    is-deeply (L: for ^5 { last L if $_ == 3; $_ }).head(4).List, (0, 1, 2),
        'a labeled last in a for loop gives no value when values are taken one at a time';
}
{
    is-deeply (L: for ^5 { NEXT { }; next L if $_ == 2; $_ }).List, (0, 1, 3, 4),
        'a labeled next in a for loop with a phaser gives no value';
}
{
    is-deeply (L: for ^5 { NEXT { }; next L if $_ == 2; $_ }).head(3).List, (0, 1, 3),
        'a labeled next in a for loop with a phaser gives no value when values are taken one at a time';
}
{
    is-deeply (L: for ^5 { NEXT { }; last L if $_ == 3; $_ }).List, (0, 1, 2),
        'a labeled last in a for loop with a phaser gives no value';
}
{
    is-deeply (L: for ^6 -> $a, $b { next L if $a == 2; $a + $b }).List, (1, 9),
        'a labeled next in a for loop taking two values gives no value';
}
{
    is-deeply (L: for ^6 -> $a, $b { last L if $a == 2; $a + $b }).List, (1,),
        'a labeled last in a for loop taking two values gives no value';
}
{
    is-deeply (L: for ^6 -> $a, $b { next L if $a == 2; $a + $b }).head(2).List, (1, 9),
        'a labeled next in a for loop taking two values gives no value when values are taken one at a time';
}
{
    is-deeply (L: for ^9 -> $a, $b, $c { next L if $a == 3; $a }).List, (0, 6),
        'a labeled next in a for loop taking three values gives no value';
}
{
    is-deeply (L: for ^9 -> $a, $b, $c { last L if $a == 3; $a }).List, (0,),
        'a labeled last in a for loop taking three values gives no value';
}
{
    is-deeply (L: for ^6 -> $a, $b { NEXT { }; next L if $a == 2; $a + $b }).List, (1, 9),
        'a labeled next in a for loop taking two values with a phaser gives no value';
}
{
    is-deeply (L: for ^6 -> $a, $b { NEXT { }; last L if $a == 2; $a + $b }).List, (1,),
        'a labeled last in a for loop taking two values with a phaser gives no value';
}
{
    is-deeply (L: for ^3 -> $x { for ^2 { next L if $x == 1 }; $x }).List, (0, 2),
        'a labeled next from an inner loop gives no value in the outer loop';
}

{
    my $i = 0;
    is-deeply (L: while $i < 4 { next L if ++$i == 2; $i }).List, (1, 3, 4),
        'a labeled next in a while loop gives no value';
}
{
    my $i = 0;
    is-deeply (L: while $i < 4 { last L if ++$i == 3; $i }).List, (1, 2),
        'a labeled last in a while loop gives no value';
}
{
    my $i = 0;
    is-deeply (L: while True { last L if ++$i == 3; next L if $i == 1; $i }).eager, (2,),
        'a labeled next and last in a while loop with a true condition give no value';
}
{
    my $i = 0;
    is-deeply (L: repeat { next L if ++$i == 2; $i } while $i < 4).List, (1, 3, 4),
        'a labeled next in a repeat loop gives no value';
}
{
    my $i = 0;
    is-deeply (L: repeat { last L if ++$i == 3; $i } while $i < 4).List, (1, 2),
        'a labeled last in a repeat loop gives no value';
}
{
    is-deeply (L: loop (my $i = 0; $i < 4; $i++) { next L if $i == 2; $i }).List, (0, 1, 3),
        'a labeled next in a loop with a condition gives no value';
}
{
    is-deeply (L: loop (my $i = 0; $i < 4; $i++) { last L if $i == 2; $i }).List, (0, 1),
        'a labeled last in a loop with a condition gives no value';
}

# vim: expandtab shiftwidth=4
