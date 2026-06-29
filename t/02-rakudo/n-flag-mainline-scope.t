use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 5;

# Under -n/-p the program runs once per input line, but its lexical
# declarations live in the compunit mainline, so they persist across lines and
# are visible to BEGIN/END. Input is two lines of 2 and 3 words (5 total).

my $in = "a b\nc d e\n";

# A BEGIN-declared variable persists and accumulates (the reported bug).
is-run 'BEGIN my $w = 0; $w += .words.elems; END say $w',
    'a BEGIN-declared variable persists across -n lines',
    :compiler-args['-n'], :in($in), :out("5\n");

# The declaration need not be inside the phaser: a plain `my` assigned in a
# BEGIN persists too.
is-run 'my $w; BEGIN { $w = 0 }; $w += .words.elems; END say $w',
    'a plain my assigned in BEGIN persists across -n lines',
    :compiler-args['-n'], :in($in), :out("5\n");

# A plain `my $w = 0` re-runs its initializer each line, so it does not
# accumulate (per-line), matching the legacy frontend.
is-run 'my $w = 0; $w += .words.elems; END say $w',
    'a plain my = 0 resets each line',
    :compiler-args['-n'], :in($in), :out("3\n");

# A name shared by two BEGIN blocks resolves to the same mainline declaration.
is-run 'BEGIN my $w = 0; BEGIN my $n = 0; $w += .words.elems; $n++; END say "$w $n"',
    'separate BEGIN-declared variables both persist',
    :compiler-args['-n'], :in($in), :out("5 2\n");

# -p modifies and prints the (writable) topic each line.
is-run 's/a/A/',
    '-p modifies the topic each line',
    :compiler-args['-p'], :in($in), :out("A b\nc d e\n");

# vim: expandtab shiftwidth=4
