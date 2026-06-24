use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 4;

# A `will leave` / `will enter` phaser trait on a top level variable must
# attach to the compilation unit's mainline block. Previously the mainline
# had no enclosing block on the attach-target stack, so the trait found no
# code object and died "No such method 'add_phaser' for invocant of type 'Mu'".

is-run q|my $x will leave { say "leave $_" } = 42; say "body $x";|,
    'will leave on a top level variable runs at mainline exit',
    :out("body 42\nleave 42\n");

is-run q|my $x will enter { say "enter" } = 1; say "body";|,
    'will enter on a top level variable runs before the mainline body',
    :out("enter\nbody\n");

is-run q|sub f { my $x will leave { say "leave $_" } = 9; say "in f" }; f(); say "after";|,
    'will leave inside a routine still runs on routine exit',
    :out("in f\nleave 9\nafter\n");

is-run q|my $x will enter { say "enter" } will leave { say "leave $_" } = 7; say "body $x";|,
    'will enter and will leave on one top level variable bracket the mainline body',
    :out("enter\nbody 7\nleave 7\n");

# vim: expandtab shiftwidth=4
