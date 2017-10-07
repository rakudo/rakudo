unit module Test::Helpers;
use Test;

sub is-run (
    Str() $code, $desc = "$code runs",
    Stringy :$in, :@compiler-args, :@args, :$out, :$err, :$status
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

sub is-run-repl ($code, $desc, :$out, :$err) is export {
    my $proc = &CORE::run( $*EXECUTABLE, :in, :out, :err );
    $proc.in.print: $code;
    $proc.in.close;
    subtest {
        plan +($out, $err).grep: *.defined;
        with $out {
            my $output    = $proc.out.slurp;
            my $test-name = 'stdout is correct';
            when Str      { is      $output, $_, $test-name; }
            when Regex    { like    $output, $_, $test-name; }
            when Callable { ok   $_($output),    $test-name; }

            die "Don't know how to handle :out of type $_.^name()";
        }

        with $err {
            my $output    = $proc.err.slurp;
            my $test-name = 'stderr is correct';
            when Str      { is      $output, $_, $test-name; }
            when Regex    { like    $output, $_, $test-name; }
            when Callable { ok   $_($output),    $test-name; }

            die "Don't know how to handle :err of type $_.^name()";
        }
    }, $desc;
}

multi doesn't-hang (Str $args, $desc, :$in, :$wait = 1.5, :$out, :$err)
is export {
    doesn't-hang \($*EXECUTABLE, '-e', $args), $desc,
        :$in, :$wait, :$out, :$err;
}

multi doesn't-hang (
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
