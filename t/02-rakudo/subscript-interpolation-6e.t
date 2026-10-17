use v6.e.PREVIEW;
use Test;

plan 13;

my @a = 10, 20, 30;
my @b = [0, 2],;
my @i = 0,;
is @a[@b[||@i]], 30,
    'a || in a nested subscript does not make the outer subscript interpolate';
sub f(*@x) { @x }
is-deeply @a[f(||(1, 0))], (20, 10),
    'a || in the arguments of a call does not make the subscript interpolate';
is-deeply @a[(||(1, 0))], (20, 10),
    'a || in parentheses does not make the subscript interpolate';
my @m = [5, 6], [7, 8];
my @p = 0, 1;
is-deeply @a[@m[||@p].grep(* > 10)], (),
    'an empty slice from a nested subscript that interpolates takes nothing';
my $c = True;
is-deeply @a[||(1, 0) if $c], (20, 10),
    'a || in a statement with a modifier does not make the subscript interpolate';
is-deeply @a[||(1, 0), 2 if $c], (20, 10, 30),
    'a || in a comma list with a modifier does not make the subscript interpolate';
is-deeply @a[||(1, 0) for 1], (20, 10),
    'a || in a statement with a loop modifier does not make the subscript interpolate';
constant always = True;
is-deeply @a[||(1, 0), 2 if always], (20, 10, 30),
    'a || in a statement with a modifier that always applies does not make the subscript interpolate';
my $f = False;
is-deeply @a[||(1, 0) if $f], (),
    'a || in a statement with a modifier that does not apply takes nothing';
is-deeply @a[||*], @a[|*],
    'a || on a Whatever curries as a | on one does';
is-deeply @a[||(* - 1)], @a[|(* - 1)],
    'a || on a WhateverCode curries as a | on one does';
my @j = 1,;
is-deeply @m[||@j [,] 0], (7,),
    'a || in a comma list written with [,] interpolates as one in a comma list does';
my %h = a => 1, b => 2;
my %g = x => ('a', 'b');
my @k = 'x',;
is-deeply %h{%g{||@k}.list}, (1, 2),
    'a || in a nested hash subscript does not make the outer hash subscript interpolate';

# vim: expandtab shiftwidth=4
