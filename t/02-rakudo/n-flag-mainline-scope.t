use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;
use nqp;

my $rakuast = nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

plan 16;

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

# A hoisted declaration's slot lives in the mainline, so the lexical-to-local
# lowering must leave it addressable by name from the loop body.
is-run 'my $x = 1; say $x',
    'a declared and used variable compiles and runs each line',
    :compiler-args['-n'], :in($in), :out("1\n1\n");

# A signature declaration hoists through its parameter targets.
is-run 'my ($a, $b) = 1, 2; say $a + $b',
    'a my list declaration works under -n',
    :compiler-args['-n'], :in($in), :out("3\n3\n");

# -p modifies and prints the (writable) topic each line.
is-run 's/a/A/',
    '-p modifies the topic each line',
    :compiler-args['-p'], :in($in), :out("A b\nc d e\n");

# A CATCH handler covers one line's iteration: a handled exception ends that
# line and the loop continues with the next one.
is-run 'CATCH { default { say "caught" } }; die "x" if $_ eq "a b"; say $_',
    'a handled exception continues with the next line',
    :compiler-args['-n'], :in($in), :out("caught\nc d e\n");

# A CONTROL handler covers one line's iteration the same way.
is-run 'CONTROL { when CX::Warn { say "ctl" } }; warn "w" if $_ eq "a b"; say $_',
    'a handled control exception continues with the next line',
    :compiler-args['-n'], :in($in), :out("ctl\nc d e\n");

# A matched when succeeds out of one line's iteration only, so the loop
# continues with the next line.
is-run 'when "a b" { say "matched" }; say "not-$_"',
    'a matched when ends only that line',
    :compiler-args['-n'], :in($in), :out("matched\nnot-c d e\n");

# The program's FIRST/NEXT/LAST phasers become loop phasers of the wrapper
# loop. The legacy frontend never runs them under -n.
if $rakuast {
    # FIRST runs during the first line's iteration, seeing its topic.
    is-run 'FIRST .say',
        'FIRST under -n runs with the first line as topic',
        :compiler-args['-n'], :in($in), :out("a b\n");

    # NEXT runs after every line's iteration.
    is-run 'NEXT .say',
        'NEXT under -n runs after each line',
        :compiler-args['-n'], :in($in), :out($in);

    # LAST runs once after the final line's iteration.
    is-run 'LAST .say',
        'LAST under -n runs with the last line as topic',
        :compiler-args['-n'], :in($in), :out("c d e\n");

    # FIRST assigns into a mainline-hoisted variable that then accumulates.
    is-run 'my $c; FIRST $c = 10; $c++; LAST say $c',
        'a FIRST-assigned variable persists and accumulates',
        :compiler-args['-n'], :in($in), :out("12\n");

    # The FIRST value variable lives on the compilation unit when no block
    # encloses the phaser, so mainline FIRST keeps its return value.
    is-run 'my $v = FIRST 42; say $v',
        'FIRST at the compilation unit mainline produces its value',
        :out("42\n");

    # A default statement sees the line as its topic.
    is-run 'when "a b" { say "isAB" }; default { say "d-$_" }',
        'a default statement sees each line as topic',
        :compiler-args['-n'], :in($in), :out("isAB\nd-c d e\n");
}
else {
    skip '-n loop phasers need the RakuAST frontend', 6;
}

# vim: expandtab shiftwidth=4
