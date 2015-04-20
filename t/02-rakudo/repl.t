use v6;
use Test;

plan 14;

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
    is qqx[$cmd].trim-trailing.lines, 'foo', '>>.() does not crash in REPL';
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
    return qqx[$repl-input | $*EXECUTABLE].trim-trailing;
}

my @input-lines;
# RT #123187
{
    @input-lines[0] = 'my int $t=4; $t.say;';
    @input-lines[1] = '$t.say';
    is feed_repl_with( @input-lines ).lines, (4, 4),
        'can use native typed variable on subsequent lines (1)';
}

{
    @input-lines = q:to/END/.split("\n");
    if False {
        say ":(";
    }
    else {
        say ":)";
    }
    END

    #?rakudo todo "indent styles don't parse right"
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

    #?rakudo todo "indent styles don't parse right"
    is feed_repl_with( @input-lines ).lines, ":)",
        "open brace on next line is parsed correctly";

    @input-lines = q:to/END/.split("\n");
    if False { say ":("; }
    else { say ":)"; }
    END

    #?rakudo todo "indent styles don't parse right"
    is feed_repl_with( @input-lines ).lines, ":)",
        "cuddled else is parsed correctly";

    @input-lines = q:to/END/.split("\n");
    if False {
        say ":(";
    } else {
        say ":)";
    }
    END

    #?rakudo todo "indent styles don't parse right"
    is feed_repl_with( @input-lines ).lines, ":)",
        "cuddled else is parsed correctly";
}

{
    @input-lines = 'say "works"', 'if True;';
    #?rakudo todo "statement mod if on the next line"
    is feed_repl_with( @input-lines ).lines, "works",
        "statement mod if on the next line works";

    @input-lines = 'say "works"', 'for 1;';
    #?rakudo todo "statement mod for on the next line"
    is feed_repl_with( @input-lines ).lines, "works",
        "statement mod for on the next line works";

    @input-lines = 'sub f { 42 }', 'f()';
    #?rakudo todo "block parsing broken"
    is feed_repl_with( @input-lines ).lines, "42",
        "single-line sub declaration works";

    @input-lines = 'sub f {', '42', '}';
    #?rakudo todo "block parsing broken"
    is feed_repl_with( @input-lines ).lines, "42",
        "single-line sub declaration works";
}

# RT #122914
{
    @input-lines = 'my $a := 42; say 1', '$a.say';
    is feed_repl_with( @input-lines ).lines, (1, 42),
        'Binding to a Scalar lasts to the next line';

    @input-lines = 'my @a := 1, 2, 3; say 1', '@a.elems.say';
    is feed_repl_with( @input-lines ).lines, (1, 3),
        'Binding to an Array lasts to the next line';

    @input-lines = 'my \a = 100; say 1', 'a.say';
    is feed_repl_with( @input-lines ).lines, (1, 100),
        'Binding to a sigilless lasts to the next line';
}
