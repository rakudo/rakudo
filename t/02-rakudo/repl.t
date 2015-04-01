use v6;
use Test;

plan 6;

# Sanity check that the repl is working at all.
my $cmd = $*DISTRO.is-win
    ?? "echo exit(42)   | $*EXECUTABLE 1>&2"
    !! "echo 'exit(42)' | $*EXECUTABLE >/dev/null 2>&1";
is shell($cmd).exit, 42, 'exit(42) in executed REPL got run';

# RT #104514
{
    my $cmd = $*DISTRO.is-win
        ?? q[echo my @a = -^^^> { say "foo" }; @a^^^>^^^>.() | ] ~ $*EXECUTABLE
        !! q[echo 'my @a = -> { say "foo" }; @a>>.()' | ] ~ $*EXECUTABLE;
    is qqx[$cmd].lines, 'foo', '>>.() does not crash in REPL';
}

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
    ## warning: works only with simple input lines which don't need quoting for Windows
    my $repl-input = '(' ~ (@lines.map: { 'echo ' ~ $quote ~ $_ ~ $quote }).join($separator) ~ ')';
    return qqx[$repl-input | $*EXECUTABLE];
}

# RT #123187
{
    my @input-lines;
    @input-lines[0] = 'my int $t=4; $t.say;';
    @input-lines[1] = '$t.say';
    is feed_repl_with( @input-lines ).lines, (4, 4),
        'can use native typed variable on subsequent lines (1)';
}

#rakudo todo "some indent styles don't parse right"
{
    my @input-lines = q:to/END/.split("\n");
    if False {
        say ":(";
    }
    else {
        say ":)";
    }
    END
    is feed_repl_with( @input-lines ).lines, ":)",
        "uncuddled else is parsed correctly";

    @input-lines = q:to/END/.split("\n");
    if False
    {
        say ":(";
    }
    else
    {
        say ":)";
    }
    END
    is feed_repl_with( @input-lines ).lines, ":)",
        "open brace on next line is parsed correctly";

    @input-lines = q:to/END/.split("\n");
    if False {
        say ":(";
    } else {
        say ":)";
    }
    END
    is feed_repl_with( @input-lines ).lines, ":)",
        "cuddled else is parsed correctly";
}
