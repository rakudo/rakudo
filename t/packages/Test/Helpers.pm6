unit module Test::Helpers;
use Test;

sub is-run (
    Str() $code, $desc = "$code runs",
    Stringy :$in, :@compiler-args, :@args, :$out = '', :$err = '', :$status = 0
) is export {
    with run :in, :out, :err,
        $*EXECUTABLE, @compiler-args, '-e', $code, @args
    {
        $in ~~ Blob ?? .in.write: $in !! .in.print: $in if $in;
        $ = .in.close;
        my $proc-out    = .out.slurp: :close;
        my $proc-err    = .err.slurp: :close;
        my $proc-status = .status;

        my $wanted-status = $status // 0;
        my $wanted-out    = $out    // '';
        my $wanted-err    = $err    // '';

        subtest $desc => {
            plan 3;
            cmp-ok $proc-out,    '~~', $wanted-out,    'STDOUT';
            cmp-ok $proc-err,    '~~', $wanted-err,    'STDERR';
            cmp-ok $proc-status, '~~', $wanted-status, 'Status';
        }
    }
}

sub is-run-repl ($code is copy, $desc, :$out = '', :$err = '') is export {
    $code .= join: "\n" if $code ~~ Positional|Seq;
    my $proc = run $*EXECUTABLE, '--repl-mode=interactive', :in, :out, :err;
    $proc.in.print: $code;
    $proc.in.close;

    subtest {
        plan +($out, $err).grep: *.defined;

        sub run-test ($_, $output, $test-name) {
            when Str        { is      $output, $_, $test-name; }
            when Regex      { like    $output, $_, $test-name; }
            when Callable   { ok   $_($output),    $test-name or diag $output; }
            when Positional { is      $output, .join("\n")~"\n", $test-name; }
            when Map        {
                subtest "$test-name lines" => {
                    plan .elems;
                    my %lines = (1….elems) «=>» $_ with $output.lines;
                    with .<t>:delete {
                        is +%lines, $_, "expected number of lines";
                    }
                    for $_<> -> (:key($ln), :value($expected)) {
                        with %lines{$ln.substr: 1} {
                            is $_, $expected, "line #$ln";
                        }
                        else {
                            flunk "No line #$ln (note: numebering starts at 1)";
                        }
                    }
                }
            }
            die "Don't know how to handle test of type $_.^name()";
        }

        run-test $_, ($*REPL-SCRUBBER//{$_})($proc.out.slurp),
            'stdout is correct' with $out;
        run-test $_, $proc.err.slurp, 'stderr is correct' with $err;
    }, $desc;
}

multi sub doesn't-hang (Str $args, $desc, :$in, :$wait = 1.5, :$out, :$err)
is export {
    doesn't-hang \($*EXECUTABLE, '-e', $args), $desc,
        :$in, :$wait, :$out, :$err;
}

multi sub doesn't-hang (
    Capture $args, $desc = 'code does not hang',
    :$in, :$wait = 1.5, :$out, :$err,
) is export {
    my $prog = Proc::Async.new: |$args;
    my ($stdout, $stderr) = '', '';
    $prog.stdout.tap: { $stdout ~= $^a };
    $prog.stderr.tap: { $stderr ~= $^a };

    # We start two Promises: the program to run and a Promise that waits for
    # $wait seconds. We await any of them, so if the $wait seconds pass,
    # await returns and we follow the path that assumes the code we ran hung.
    my $promise = $prog.start;
    await $prog.write: $in.encode if $in.defined;
    await Promise.anyof: Promise.in($wait * (%*ENV<ROAST_TIMING_SCALE>//1)),
                         $promise;

    my $did-not-hang = False;
    given $promise.status {
        when Kept { $did-not-hang = True };
        $prog.kill;
    }

    subtest $desc, {
        plan 1 + ( $did-not-hang ?? ($out, $err).grep(*.defined) !! 0 );
        ok $did-not-hang, 'program did not hang';
        if $did-not-hang {
            cmp-ok $stdout, '~~', $out, 'STDOUT' if $out.defined;
            cmp-ok $stderr, '~~', $err, 'STDERR' if $err.defined;
        }
    };
}
