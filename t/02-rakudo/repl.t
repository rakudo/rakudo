use v6;
use lib <t/packages>;
use Test;
use Test::Helpers;

# Sanity check that the repl is working at all.
my $cmd = $*DISTRO.is-win
    ?? "echo exit(42)   | $*EXECUTABLE 1>&2"
    !! "echo 'exit(42)' | $*EXECUTABLE >/dev/null 2>&1";
is shell($cmd).exitcode, 42, 'exit(42) in executed REPL got run';

# RT #104514
{
    my $cmd = $*DISTRO.is-win
        ?? q[echo my @a = -^^^> { say "foo" }; @a^^^>^^^>.() | ] ~ $*EXECUTABLE
        !! q[echo 'my @a = -> { say "foo" }; @a>>.()' | ] ~ $*EXECUTABLE;
    like qqx[$cmd].Str, /"foo" $$/, '>>.() does not crash in REPL';
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

sub feed_repl_with ( @lines, Bool:D :$no-filter-messages = False ) {
    ## warning: works only with simple input lines which don't need quoting for Windows
    my $repl-input = '(' ~ (@lines.map: { 'echo ' ~ $quote ~ $_ ~ $quote }).join($separator) ~ ')';
    my $repl-output = qqx[$repl-input | $*EXECUTABLE].trim-trailing;
    unless $no-filter-messages {
        $repl-output ~~ s/^^ "You may want to `zef install Readline` or `zef install Linenoise` or use rlwrap for a line editor\n\n"//;
        $repl-output ~~ s/^^ "To exit type 'exit' or '^D'\n"//;
    }
    $repl-output ~~ s:g/ ^^ "> " //; # Strip out the prompts
    $repl-output ~~ s:g/ ">" $ //; # Strip out the final prompt
    $repl-output ~~ s:g/ ^^ "* "+ //; # Strip out the continuation-prompts
    $repl-output
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

    todo "indent styles don't parse right";
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

    todo "indent styles don't parse right";
    is feed_repl_with( @input-lines ).lines, ":)",
        "open brace on next line is parsed correctly";

    @input-lines = q:to/END/.split("\n");
    if False { say ":("; }
    else { say ":)"; }
    END

    todo "indent styles don't parse right";
    is feed_repl_with( @input-lines ).lines, ":)",
        "cuddled else is parsed correctly";

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

{
    @input-lines = 'say "works"', 'if True;';
    todo "statement mod if on the next line";
    is feed_repl_with( @input-lines ).lines, "works",
        "statement mod if on the next line works";

    @input-lines = 'say "works"', 'for 1;';
    todo "statement mod for on the next line";
    is feed_repl_with( @input-lines ).lines, "works",
        "statement mod for on the next line works";

    @input-lines = 'sub f { 42 }', 'f()';
    todo "block parsing broken";
    is feed_repl_with( @input-lines ).lines, "42",
        "single-line sub declaration works";

    @input-lines = 'sub f {', '42', '}';
    todo "block parsing broken";
    is feed_repl_with( @input-lines ).lines, "42",
        "single-line sub declaration works";
}

# RT #122914
{
    @input-lines = 'my $a = 42; say 1', '$a.say';
    is feed_repl_with( @input-lines ).lines, (1, 42),
        'Assigning to a Scalar lasts to the next line';

    @input-lines = 'my @a = 1, 2, 3; say 1', '@a.elems.say';
    is feed_repl_with( @input-lines ).lines, (1, 3),
        'Assigning to an Array lasts to the next line';

    @input-lines = 'my \a = 100; say 1', 'a.say';
    is feed_repl_with( @input-lines ).lines, (1, 100),
        'Assigning to a sigilless lasts to the next line';
}

{
    @input-lines = '';
    is feed_repl_with(@input-lines).lines, (),
        'Entering a blank line gives back the prompt';

    @input-lines = '""';
    is feed_repl_with(@input-lines).lines, (''),
        'An empty string gives back one blank line';
}

{
    @input-lines = '}';
    like feed_repl_with(@input-lines), / "===" "\e[0m"? "SORRY!" "\e[31m"? "===" /,
        'Syntax error gives a compile-time error';
    like feed_repl_with(@input-lines), / "Unexpected closing bracket" /,
        'Syntax error gives the expected error';

    @input-lines = 'sub }', 'say 1+1';
    like feed_repl_with(@input-lines),
        / "===" "\e[0m"? "SORRY!" "\e[31m"? "===" /,
        'Syntax error gives a compile-time error';
    like feed_repl_with(@input-lines), / "Missing block" /,
        'Syntax error gives the expected error';
    is   feed_repl_with(@input-lines).comb('Error while compiling').elems, 1,
        'Syntax error clears on further input';

    @input-lines = 'this-function-does-not-exist()';
    like feed_repl_with(@input-lines), / "===" "\e[0m"? "SORRY!" "\e[31m"? "===" /,
        'EVAL-time compile error gives a compile-time error';
    like feed_repl_with(@input-lines), / "Undeclared routine" /,
        'EVAL-time compile error error gives the expected error';

    @input-lines = 'sub f { this-function-does-not-exist() } ; f()';
    like feed_repl_with(@input-lines), / "Undeclared routine" /,
        'EVAL-time compile error error gives the expected error';

    @input-lines = '[1].map:{[].grep:Str}';
    like feed_repl_with(@input-lines), / "Cannot resolve caller" /,
        'Print-time error error gives the expected error';
}

{
    for <return redo next last proceed succeed> -> $cmd {
        @input-lines = $cmd;
        like feed_repl_with(@input-lines), / "Control flow commands not allowed in topleve" /,
            "Raises error when you run control flow command '$cmd'";
    }

    like feed_repl_with(['emit 42']), /'emit without'/,
        '`emit` prints useful message';

    like feed_repl_with(['take 42']), /'take without'/,
        '`take` prints useful message';

    like feed_repl_with(['warn "foo"']), /'foo'/,
        'Warnings print their message';
}

# RT#130876
{
    like feed_repl_with(['say "hi"; die "meows";']), /meows/,
        'previous output does not silence exceptions';

    my $out = feed_repl_with
        ['say "hi"; my $f = Failure.new: "meows"; $f.Bool; $f'];
    ok $out.contains('meows').not,
        'previous output prevents output of handled failures';

    $out = feed_repl_with ['say "hi"; X::AdHoc.new(:payload<meows>)'];
    ok $out.contains('meows').not,
        'previous output prevents output of unthrown exceptions';

    $out = feed_repl_with ['say "hi"; try +"a"; $!'];
    ok $out.contains('meows').not,
        'previous output does not prevent output of unthrown exceptions';

    $out = feed_repl_with([
          ｢say "hi"; use nqp; my $x = REPL.new(nqp::getcomp("perl6"), %)｣
        ~ ｢.repl-eval(q|die "meows"|, $);｣
    ]);
    ok $out.contains('meows').not,
        ｢can't trick REPL into thinking an exception was thrown (RT#130876)｣;
}

# RT#130874
like feed_repl_with(['Nil']), /Nil/, 'REPL outputs Nil as a Nil';


# Since there might be some differences in REPL sessions in whitespace
# or what not, strip all \W and then check what we have left over is what
# a normal session should have. This lets us catch any unexpected error
# messages and stuff.
is feed_repl_with(['say "hi"'], :no-filter-messages).subst(:g, /\W+/, ''),
    'YoumaywanttozefinstallReadlineorzefinstallLinenoise'
    ~ 'oruserlwrapforalineeditor' ~ 'ToexittypeexitorD' ~ 'hi',
'REPL session does not have unexpected stuff';

## XXX TODO: need to write tests that exercise the REPL with Linenoise
# and Readline installed. Particular things to check:
# 1. History file can be made on all OSes:
#    https://github.com/rakudo/rakudo/commit/b4fa6d6792dd02424d2182b73c31a071cddc0b8e
# 2. Test REPL does not show errors when $*HOME is not set:
#    https://rt.perl.org/Ticket/Display.html?id=130456

# RT #119339
{
    todo 'make the function check STDERR', 2;
    like feed_repl_with(['say 069']),
        /'Potential difficulties:'
            .* 'Leading 0' .+ "use '0o' prefix,"
            .* '69 is not a valid octal number'/,
        'prefix 0 on invalid octal warns in REPL';

    like feed_repl_with(['say 069']),
        /'Potential difficulties:'
            .* 'Leading 0' .+ "use '0o' prefix,"
            .* '0o67 is not a valid octal number'/,
        'prefix 0 on valid octal warns in REPL';
}

# RT #70297
{
    my $proc = &CORE::run( $*EXECUTABLE, :in, :out, :err);
    $proc.in.close;

    skip 'Result differs on OSX';
    subtest {
        plan 2;
        is   $proc.err.slurp, '', 'stderr is correct';
        like $proc.out.slurp, /"To exit type 'exit' or '^D'\n> "/,
            'stdout is correct';
    }, 'Pressing CTRL+D in REPL produces correct output on exit';
}

# RT #128470
{
    my $code-to-run = q/[1..99].map:{[$_%%5&&'fizz', $_%%3&&'buzz'].grep:Str}/
        ~ "\nsay 'We are still alive';\n";

    is-run-repl $code-to-run,
        out => /'Cannot resolve caller grep' .* 'We are still alive'/,
        err => '',
        'exceptions from lazy-evaluated things do not crash REPL';
}

# RT #127933
{
    my $code = [~]  'my ( int8 $a,  int16 $b,  int32 $c,  int64 $d,',
                        'uint8 $e, uint16 $f, uint32 $g, uint64 $h,',
                                              'num32 $i,  num64 $j,',
                    ') = 1, 2, 3, 4, 5, 6, 7, 8, 9e0, 10e0;';

    todo 'RT#127933';
    is-run-repl "$code\nsay 'test is good';\n",
        :err(''),
        :out(/'(1 2 3 4 5 6 7 8 9 10)' .* 'test is good'/),
    'Using native numeric types does not break REPL';
}

# RT #128595
{
    # REPL must not start, but if it does start and wait for input, it'll
    # "hang", from our point of view, which the test function will detect
    doesn't-hang \(:w, $*EXECUTABLE, '-M', "NonExistentModuleRT128595"),
        :out(/^$/),
        :err(/'Could not find NonExistentModuleRT128595'/),
    'REPL with -M with non-existent module does not start';
}

# RT #128973
{
    is-run-repl "my \$x = 42;\nsay qq/The value is \$x/;\n",
        :err(''),
        :out(/'The value is 42'/),
    'variables persist across multiple lines of input';
}

{
    # If the REPL evaluates all of the previously-entered code on each
    # entered line of code, then we'll have more than just two 'say' print
    # outs. So we check the output each output happens just once
    my $code = <one two>.map({ "say 'testing-repl-$_';"}).join("\n");
    is-run-repl "$code\n",
        :err(''),
        :out({
                $^o.comb('testing-repl-one') == 1
            and $^o.comb('testing-repl-two') == 1
        }),
    'previously-entered code must not be re-run on every line of input';
}

# RT #125412
{
    my $code = 'sub x() returns Array of Int { return my @x of Int = 1,2,3 };'
        ~ "x().WHAT.say\n";

    is-run-repl $code x 10,
        :err(''),
        :out({ not $^o.contains: '[Int][Int]' }),
    'no bizzare types returned from redeclared "returns an `of` Array" sub';
}

# RT #127631
{

    is-run-repl join("\n", <last next redo>, 'say "rt127631-pass"', ''),
        :err(''),
        :out(/'rt127631-pass'/),
    'loop controls do not exit the REPL';
}

# RT #130719
{
    is-run-repl join("\n", 'Mu', ''),
        :err(''),
        :out{.contains('failed').not},
    ｢REPL can handle `Mu` as line's return value｣;
}
