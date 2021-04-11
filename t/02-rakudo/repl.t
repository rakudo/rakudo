use v6;
use lib <t/packages>;
use Test;
use Test::Helpers;

plan 47;

my $eof = $*DISTRO.is-win ?? "'^Z'" !! "'^D'";
my $*REPL-SCRUBBER = -> $_ is copy {
    $_ = .lines.skip(4).join("\n");
    s/^^ "You may want to `zef install Readline` or `zef install Linenoise`"
        " or use rlwrap for a line editor\n\n"//;
    s/^^ "To exit type 'exit' or $eof\n"//;
    s:g/ ^^ "> "  //; # Strip out the prompts
    s:g/    ">" $ //; # Strip out the final prompt
    s:g/ ^^ "* "+ //; # Strip out the continuation-prompts
    $_
}

{
    (temp %*ENV)<RAKUDO_ERROR_COLOR  RAKUDO_LINE_EDITOR>:delete;
    subtest 'sanity check; load without tweaking line editor' => {
        plan 3;
        my $p := run $*EXECUTABLE, '--repl-mode=interactive', :in, :out, :err;
        $p.in.say: '133742.flip.say';
        $p.in.close;
        like $p.out.slurp(:close),     /247331/, 'result of code is on STDOUT';
        is $p.err.slurp(:close).chars, 0,        'no STDERR output';
        is $p.exitcode,                0,        'successful exit code';
    }
}

# https://github.com/Raku/old-issue-tracker/issues/5245
is-run-repl «'my int $t=4; $t.say;'  '$t.say'», <4 4>,
    'can use native typed variable on subsequent lines (1)';

subtest 'indented code parses correctly' => {
    plan 4;

    todo "indent styles don't parse right", 3;
    is-run-repl q:to/END/,
        if False {
            say ":(";
        }
        else {
            say ":)";
        }
        END
    ":)\n", 'uncuddled else is parsed correctly';

    is-run-repl q:to/END/,
        if False
        {
            say ":(";
        }
        else
        {
            say ":)";
        }
        END
    ":)\n", 'open brace on next line is parsed correctly';

    is-run-repl q:to/END/,
        if False { say ":("; }
        else { say ":)"; }
        END
    ":)\n", 'partially-cuddled else is parsed correctly';

    is-run-repl q:to/END/,
        if False {
            say ":(";
        } else {
            say ":)";
        }
        END
    ":)\n", 'cuddled else';
}

is-run-repl «'sub f {'  'say "works"'  '}'  'f()'», {
    .lines == 2 and .lines.tail eq 'works'
}, 'multi-line sub decl';
is-run-repl «'sub f { say "works" }'  'f()'», {
    .lines == 2 and .lines.tail eq 'works'
}, 'single-line sub declaration works';

# https://github.com/Raku/old-issue-tracker/issues/3543
subtest 'assignment maintains values on subsequent lines' => {
    plan 4;
    is-run-repl «'my $a = 42; say 1'  '$a.say'», "1\n42\n",
        'Scalar';
    is-run-repl «'my @a = 1, 2, 3; say 1'  '@a.elems.say'», "1\n3\n",
        'Array';
    is-run-repl «'my %h = 1..4; say 1'  'say +%h.keys'», "1\n2\n",
        'Hash';
    is-run-repl «'my \a = 100; say 1'  'a.say'», "1\n100\n",
        'sigilless value';
}

skip 'Different result on OSX';
#is-run-repl   "\n", '> ', 'entering a blank line gives back the prompt';
is-run-repl "''\n", "\n", 'an empty string gives back one blank line';

is-run-repl "}\n", /'===SORRY!===' .* 'Unexpected closing bracket'/,
    'syntax error gives a compile-time error';

is-run-repl "}\nsay 42", {
    .match: /'===SORRY!===' .* 'Unexpected closing bracket' .* '42'/
    and 1 == .comb: 'Error while compiling'
}, 'syntax error clears on further input';

is-run-repl ['meow-meow()'], /'===SORRY!===' .* 'Undeclared routine'/,
    'undeclared routines give compile time errors';
is-run-repl ['sub f { meow-meow() }; f()'],
    /'===SORRY!===' .* 'Undeclared routine'/,
    'undeclared routines inside another routine give compile time errors';
is-run-repl ['[1].map:{[].grep:Str}'], /'Cannot resolve caller'/,
    'run-time error error gives the expected error';


for <return redo next last proceed succeed> -> $cmd {
    is-run-repl [$cmd], /'Control flow commands not allowed in toplevel'/,
        "raises error when you run control flow command '$cmd' in top level";
}

is-run-repl ['emit 42'   ], /'emit without'/, '`emit` errors usefully';
is-run-repl ['take 42'   ], /'take without'/, '`take` errors usefully';
is-run-repl ['warn "foo"'], /'foo'         /, 'warn() shows warnings';

# https://github.com/Raku/old-issue-tracker/issues/6106
{
    is-run-repl ['say "hi"; die "meows";'], :out(/meows/),
        'previous output does not silence exceptions';

    is-run-repl ['say "hi"; my $f = Failure.new: "meows"; $f.Bool; $f'],
        *.contains('meows').not,
        'previous output prevents output of handled failures';

    is-run-repl ['say "hi"; X::AdHoc.new(:payload<meows>)'],
        *.contains('meows').not,
        'previous output prevents output of unthrown exceptions';

    is-run-repl ['say "hi"; try +"a"; $!'],
        *.contains('meows').not,
        'previous output does not prevent output of unthrown exceptions';

    is-run-repl [
          ｢say "hi"; use nqp; my $x = REPL.new(nqp::getcomp("perl6"), %)｣
        ~ ｢.repl-eval(q|die "meows"|, $);｣
    ], *.contains('meows').not,
        ｢can't trick REPL into thinking an exception was thrown｣;
}



# https://github.com/Raku/old-issue-tracker/issues/6104
is-run-repl ['Nil'], /Nil/, 'REPL outputs Nil as a Nil';

# Since there might be some differences in REPL sessions in whitespace
# or what not, strip all \W and then check what we have left over is what
# a normal session should have. This lets us catch any unexpected error
# messages and stuff.
{
    my $*REPL-SCRUBBER := Nil;
    skip 'Result differs on OSX';
    # is-run-repl ['say "hi"'], {
    #     .subst(:g, /\W+/, '') eq
    #     'YoumaywanttozefinstallReadlineorzefinstallLinenoise'
    #     ~ 'oruserlwrapforalineeditor' ~ 'ToexittypeexitorD' ~ 'hi'
    # }, 'REPL session does not have unexpected stuff';

    ## XXX TODO: need to write tests that exercise the REPL with Linenoise
    # and Readline installed. Particular things to check:
    # 1. History file can be made on all OSes:
    #    https://github.com/rakudo/rakudo/commit/b4fa6d6792dd02424d2182b73c31a071cddc0b8e
    # 2. Test REPL does not show errors when $*HOME is not set:
    #    https://rt.perl.org/Ticket/Display.html?id=130456
}

# https://github.com/Raku/old-issue-tracker/issues/3211
{
    is-run-repl ['say 069'], :out("69\n"), :err(/'Potential difficulties:'
        .* 'Leading 0' .+ "use '0o' prefix,"
        .* '69 is not a valid octal number'
    /), 'prefix 0 on invalid octal warns in REPL';

    is-run-repl ['say 067'], :out("67\n"), :err(/'Potential difficulties:'
        .* 'Leading 0' .+ "use '0o' prefix" .* "like, '0o67'"
    /), 'prefix 0 on valid octal warns in REPL';
}

# https://github.com/Raku/old-issue-tracker/issues/1393
{
    my $proc = run $*EXECUTABLE, :in, :out, :err;
    $proc.in.close;

    skip 'Result differs on OSX';
    # subtest {
    #     plan 2;
    #     is   $proc.err.slurp, '', 'stderr is correct';
    #     like $proc.out.slurp, /"To exit type 'exit' or '^D'\n> "/,
    #         'stdout is correct';
    # }, 'Pressing CTRL+D in REPL produces correct output on exit';
}

# https://github.com/Raku/old-issue-tracker/issues/5399
{
    my $code-to-run = q/[1..99].map:{[$_%%5&&'fizz', $_%%3&&'buzz'].grep:Str}/
        ~ "\nsay 'We are still alive';\n";

    is-run-repl $code-to-run,
        out => /'Cannot resolve caller grep' .* 'We are still alive'/,
        err => '',
        'exceptions from lazy-evaluated things do not crash REPL';
}

# https://github.com/Raku/old-issue-tracker/issues/5245
{
    my $code = [~]  'my ( int8 $a,  int16 $b,  int32 $c,  int64 $d,',
                        'uint8 $e, uint16 $f, uint32 $g, uint64 $h,',
                                              'num32 $i,  num64 $j,',
                    ') = 1, 2, 3, 4, 5, 6, 7, 8, 9e0, 10e0;';

    todo 'https://github.com/rakudo/rakudo/issues/4161';
    is-run-repl "$code\nsay 'test is good';\n",
        :err(''),
        :out(/'(1 2 3 4 5 6 7 8 9 10)' .* 'test is good'/),
    'Using native numeric types does not break REPL';
}

# https://github.com/Raku/old-issue-tracker/issues/5444
{
    # REPL must not start, but if it does start and wait for input, it'll
    # "hang", from our point of view, which the test function will detect
    doesn't-hang \(:w, $*EXECUTABLE,
        '--repl-mode=interactive', '-M', 'NonExistentModuleRT128595'
    ), :out(/^$/), :err(/'Could not find NonExistentModuleRT128595'/),
    'REPL with -M with non-existent module does not start';
}

# https://github.com/Raku/old-issue-tracker/issues/5575
{
    is-run-repl "my \$x = 42;\nsay qq/The value is \$x/;\n",
        :err(''),
        :out(/'The value is 42'/),
    'variables persist across multiple lines of input';
}

# https://github.com/Raku/old-issue-tracker/issues/2917
is-run-repl "my \\a = any set <1 2 3>;\nsay 1 ~~ a",
    :out{
        .contains('any') and .contains('False') and not .contains: 'True';
    },
    'REPL does neither crash nor report True and False for junction';

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

# https://github.com/Raku/old-issue-tracker/issues/4325
{
    my $code = 'sub x() returns Array of Int { return my @x of Int = 1,2,3 };'
        ~ "x().WHAT.say\n";

    is-run-repl $code x 10,
        :err(''),
        :out({ not $^o.contains: '[Int][Int]' }),
    'no bizzare types returned from redeclared "returns an `of` Array" sub';
}

# https://github.com/Raku/old-issue-tracker/issues/5157
{

    is-run-repl join("\n", <last next redo>, 'say "rt127631-pass"', ''),
        :err(''),
        :out(/'rt127631-pass'/),
    'loop controls do not exit the REPL';
}

# https://github.com/Raku/old-issue-tracker/issues/6057
{
    is-run-repl join("\n", 'Mu', ''),
        :err(''),
        :out{.contains('failed').not},
    ｢REPL can handle `Mu` as line's return value｣;
}

# https://github.com/Raku/old-issue-tracker/issues/6667
is-run-repl "say 42; none True\n", :err(''), :out{
    .contains('42') and not .contains: 'No such method';
}, 'REPL does not explode with none Junction return values';

# https://github.com/Raku/old-issue-tracker/issues/3254
is-run-repl '"asa" ~~ /:s ^a* %',
    :out{.contains: 'Missing quantifier' and not .contains('NullPointerException')},
    'REPL detects incomplete regex (no NullPointerException)';

# https://github.com/Raku/old-issue-tracker/issues/2768
is-run-repl '$_**2',
    :out{!.contains('message') and !.contains('not found')
         and !.contains('No such method')},
    :err(''),
    'no complaints about .message';

# https://github.com/Raku/old-issue-tracker/issues/3599
is-run-repl 'say "b".subst(/(.)/,{$0~$0}); say "%20" ~~ /:i \%(<[0..9A..F]>**2)/;'
            ~ "\n" ~ 'say "a".subst(/(.)/,{$0~$0});',
    :out{.contains('bb') and .contains('aa') and not .contains('2020')},
    :err(''),
    ｢no sticky $0 values across lines｣;

# https://github.com/rakudo/rakudo/issues/1925
is-run-repl '&say.package', :out{.contains: 'GLOBAL'}, :err(''),
    ｢REPL can auto-print non-Mu things that lack .WHERE and .gist｣;

# https://github.com/rakudo/rakudo/issues/2184
is-run-repl 'my $fh = $*EXECUTABLE.open(:r)',
	:out{.contains: 'IO::Handle' and not .contains('Failed to write')},
	:err(''),
	｢no complaints about failed writing to filehandle when opening a file｣;

# vim: expandtab shiftwidth=4
