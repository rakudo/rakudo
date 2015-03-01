use v6;
use Test;

plan 3;

# Sanity check that the repl is working at all.
my $cmd = $*DISTRO.is-win
    ?? "echo exit(42)   | $*EXECUTABLE 1>&2"
    !! "echo 'exit(42)' | $*EXECUTABLE >/dev/null 2>&1";
is shell($cmd).exit, 42, 'exit(42) in executed REPL got run';

my $quote;
my $separator;
if $*DISTRO.is-win {
    $quote     = "";
    $separator = "& ";
}
else {
    $quote     = "'";
    $separator = "; ";
}

sub feed_repl_with ( @lines ) {
    my $repl-input = '(' ~ (@lines.map: { 'echo ' ~ $quote ~ $_ ~ $quote }).join($separator) ~ ')';
    return qqx[$repl-input | $*EXECUTABLE];
}

# RT #104514
{
    my @input-lines = ( 'my @a = -> { say "foo" }; @a>>.()' );
    is feed_repl_with( @input-lines ).lines, 'foo',
        '>>.() does not crash in REPL';
}

# RT #123187
{
    my @input-lines;
    @input-lines[0] = 'my int $t=4; $t.say';
    @input-lines[1] = '$t.say';
    is feed_repl_with( @input-lines ).lines, (4, 4),
        'can use native typed variable on subsequent lines (1)';
}
