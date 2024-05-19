unit module Test::Helpers;
use Test;

sub group-of (
    Pair (
        Int:D :key($plan),
        Pair  :value((
            Str:D :key($desc),
                  :value(&tests))))
) is export is test-assertion {
    subtest $desc => {
        plan $plan;
        tests
    }
}

sub is-run (
    Str() $code, $desc = "$code runs",
    Stringy :$in, :@compiler-args, :@args, :$out = '', :$err = '', :$exitcode = 0
) is export is test-assertion {
    my @proc-args = flat do if $*DISTRO.is-win {
        # $*EXECUTABLE is a batch file on Windows, that goes through cmd.exe
        # and chokes on standard quoting. We also need to remove any newlines
        <cmd.exe  /S /C>, $*EXECUTABLE, @compiler-args, '-e',
        ($code,  @args).subst(:g, "\n", " ")
    }
    else {
        $*EXECUTABLE, @compiler-args, '-e', $code, @args
    }

    with run :in, :out, :err, @proc-args {
        $in ~~ Blob ?? .in.write: $in !! .in.print: $in if $in;
        $ = .in.close;
        my $proc-out      = .out.slurp: :close;
        my $proc-err      = .err.slurp: :close;
        my $proc-exitcode = .exitcode;

        my $wanted-exitcode = $exitcode // 0;
        my $wanted-out      = $out    // '';
        my $wanted-err      = $err    // '';

        subtest $desc => {
            plan 3;
            cmp-ok $proc-out,      '~~', $wanted-out,      'STDOUT';
            cmp-ok $proc-err,      '~~', $wanted-err,      'STDERR';
            cmp-ok $proc-exitcode, '~~', $wanted-exitcode, 'Exit code';
        }
    }
}

proto sub is-run-repl(|) is export {*}
multi sub is-run-repl($code, $out, $desc, |c) is test-assertion {
    is-run-repl $code, $desc, :$out, |c;
}
multi sub is-run-repl(
  $code is copy, $desc, :$out = '', :$err = '', :$line-editor = 'none'
) is test-assertion {
    $code .= join: "\n" if $code ~~ Positional|Seq;
    (temp %*ENV)<RAKUDO_ERROR_COLOR  RAKUDO_LINE_EDITOR> = 0, $line-editor;
    my $proc = run $*EXECUTABLE, '--repl-mode=interactive', :in, :out, :err;
    $proc.in.print: $code;
    $proc.in.close;

    subtest {
        plan +($out, $err).grep: *.defined;

        sub run-test ($_, $output, $test-name) {
            when Str        { is      $output, $_, $test-name; }
            when Regex      { like    $output, $_, $test-name; }
            when Callable   { ok   $_($output),    $test-name or diag $output; }
            when Positional|Seq {
                is $output, .join("\n")~"\n", $test-name;
            }
            die "Don't know how to handle test of type $_.^name()";
        }

        run-test $_, ($*REPL-SCRUBBER//{$_})($proc.out.slurp: :close),
            'stdout is correct' with $out;
        run-test $_, $proc.err.slurp(:close), 'stderr is correct' with $err;
    }, $desc;
}

proto sub doesn't-hang(|) is export {*}
multi sub doesn't-hang(
  Str $args, $desc, :$in, :$wait = 15, :$out, :$err
) is test-assertion {
    doesn't-hang \($*EXECUTABLE.absolute, '-e', $args), $desc,
        :$in, :$wait, :$out, :$err;
}

# TODO XXX: for some reason shoving this variable inside the routine and
# using `state` instead of `my` results in it having value 0
my $VM-time-scale-multiplier = $*VM.name eq 'jvm' ?? 20/3 !! 1;
multi sub doesn't-hang (
    Capture $args, $desc = 'code does not hang',
    :$in, :$wait = 15, :$out, :$err,
) is test-assertion {
    my $prog = Proc::Async.new: |$args;
    my ($stdout, $stderr) = '', '';
    $prog.stdout.tap: { $stdout ~= $^a };
    $prog.stderr.tap: { $stderr ~= $^a };

    # We start two Promises: the program to run and a Promise that waits for
    # $wait seconds. We await any of them, so if the $wait seconds pass,
    # await returns and we follow the path that assumes the code we ran hung.
    my $promise = $prog.start;
    await $prog.write: $in.encode if $in.defined;
    await Promise.anyof: Promise.in(
        $wait * $VM-time-scale-multiplier * (%*ENV<ROAST_TIMING_SCALE>//1)
    ), $promise;

    my $did-not-hang = False;
    given $promise.status {
        when Kept { $did-not-hang = True };
        $prog.kill;
    }

    subtest $desc, {
        plan 1 + ( $did-not-hang ?? ($out, $err).grep(*.defined) !! 0 );
        ok $did-not-hang, 'program did not hang'
          or diag "\nHang in doesn't-hang() test detected by heuristic.\n"
            ~ "You can set \%*ENV<ROAST_TIMING_SCALE> to a value higher than 1\n"
            ~ "to make it wait longer.\n";
        if $did-not-hang {
            cmp-ok $stdout, '~~', $out, 'STDOUT' if $out.defined;
            cmp-ok $stderr, '~~', $err, 'STDERR' if $err.defined;
        }
    };
}

sub make-rand-path (--> IO::Path:D) {
    my $p = $*TMPDIR;
    $p.resolve.child: (
        'perl6_roast_',
        $*PROGRAM.basename, '_line',
        ((try callframe(3).code.line)||''), '_',
        rand,
        time,
    ).join.subst: :g, /\W/, '_';
}
my @FILES-FOR-make-temp-file;
my @DIRS-FOR-make-temp-dir;
END {
    unlink @FILES-FOR-make-temp-file;
    rmdir  @DIRS-FOR-make-temp-dir;
}
sub make-temp-path(|c) is export { make-temp-file |c }
sub make-temp-file
    (:$content where Any:U|Blob|Cool, Int :$chmod --> IO::Path:D) is export
{
    @FILES-FOR-make-temp-file.push: my \p = make-rand-path;
    with   $chmod   { p.spurt: $content // ''; p.chmod: $_ }
    orwith $content { p.spurt: $_ }
    p
}
sub make-temp-dir (Int $chmod? --> IO::Path:D) is export {
    @DIRS-FOR-make-temp-dir.push: my \p = make-rand-path;
    p.mkdir;
    p.chmod: $_ with $chmod;
    p
}

sub has-symbols(%stash, @expected, Str:D $desc) is export is test-assertion {
    subtest $desc, {
        plan 2;
        my @unknown;
        my @missing;
        my %expected = @expected.map: * => 1;
        @unknown.push($_) unless %expected{$_}:exists for %stash.keys;
        @missing.push($_) unless %stash{$_}:exists for %expected.keys;
        diag "Found {+@unknown} unexpected entries: { @unknown.sort }" if @unknown;
        diag "Missing {+@missing} expected entries: { @missing.sort }" if @missing;
        ok @unknown == 0, "No unexpected entries";
        ok @missing == 0, "No missing entries";
    }
}

=begin pod

=head2 group-of
group-of (Pair (Int:D :key($plan), Pair :value((Str:D :key($desc), :value(&tests)))))

A more concise way to write subtests. Code:

    group-of 42 => 'some feature' => {
        ok 1;
        ok 2;
        ...
        ok 42;
    }

Is equivalent to:

    subtest 'some feature' => {
        plan 42;
        ok 1;
        ok 2;
        ...
        ok 42;
    }

=head2 is-run

  sub is-run (
      Str() $code, $desc = "$code runs",
      Stringy :$in, :@compiler-args, :@args, :$out = '', :$err = '', :$exitcode = 0
  )

Runs code with C<Proc::Async>, smartmatching STDOUT with C<$out>,
STDERR with C<$err> and exit code with C<$exitcode>. C<$in> can be a C<Str>
or a C<Blob>. C<@args> are arguments to the program, while C<@compiler-args>
are arguments to the compiler.

=head2 is-run-repl

    multi sub is-run-repl ($code, $out, $desc, |c)
    multi sub is-run-repl ($code, $desc, :$out = '', :$err = '', :$line-editor = 'none')

Fires up the REPL and feeds it with C<$code>, setting
C«%*ENV<RAKUDO_LINE_EDITOR>» to the value of C<$line-editor> for the duration
of the test. If C<$code> is a C<Positional>
or a C<Seq>, will join each element with a C<"\n">. The C<$out> and C<$err>
test STDOUT and STDERR respectively and can be of the following types:

    Str: uses `is` test
    Regex: uses `like` test
    Callable: executes, giving string to test as argument, truthy value means pass
    Positional or Seq: assumes to be a list of lines. Joins with "\n", appends
        another "\n" to the end and uses `is` test

It's possible to scrub STDOUT of unwanted strings before testing by setting
C<$*REPL-SCRUBBER> to a C<Callable> that takes original STDOUT as argument and
returns the scrubbed version.

Note: the routine sets C«%*ENV<RAKUDO_ERROR_COLOR>» to C<0>

=head2 doesn't-hang

    doesn't-hang 'say "some code"' :out(/'some code'/),
        'some code does not hang';
    doesn't-hang \(:w, $*EXECUTABLE, '-M', "SomeNonExistentMod"),
        :in("say 'output works'\nexit\n"),
        :out(/'output works'/),
    'REPL with -M with non-existent module';

Uses C<Proc::Async> to execute a potentially-hanging program and kills it after
a specified timeout, if it doesn't surrender peacefully. Collects STDERR
and STDOUT, optional taking regex matchers for additional testing. Takes
the following arguments:

=head3 First positional argument

    'say "some code"'
    \(:w, $*EXECUTABLE, '-M', "SomeNonExistentMod")

B<Mandatory.> Can be a C<Capture> or a C<Str>. A C<Capture> represents
arguments to pass to C<Proc::Async.new()>. If C<Str> is passed, it is treated
as if a capture with C<\($*EXECUTABLE, '-e', $code-to-run)> passed, where
C<$code-to-run> is the code contained in the passed C<Str>.

=head3 Second positional argument

B<Optional.> Takes a C<Str> for test description. B<Defaults to:>
C<'code does not hang'>

=head3 C<:wait>

B<Optional.> Specifies the amount of time in seconds to wait for the
executed program to finish. B<Defaults to:> C<1.5>

=head3 C<:in>

B<Optional>. Takes a C<Str> that will be sent to executed program's STDIN.
B<By default> not specified.

=head3 C<:out>

B<Optional>. Takes a C<.defined> object that will be smartmatched against
C<Str> containing program's STDOUT. If the program doesn't finish before
C<:wait> seconds, no attempt to check STDOUT will be made. B<By default>
not specified.

=head3 C<:err>

B<Optional>. Takes a C<.defined> object that will be smartmatched against
C<Str> containing program's STDERR. If the program doesn't finish before
C<:wait> seconds, no attempt to check STDERR will be made. B<By default>
not specified.

=head2  make-temp-file(:$content, :$chmod)

    sub make-temp-file(:$content where Blob|Cool, Int :$chmod --> IO::Path:D)

Creates a semi-random path in C<$*TMPDIR>, optionally setting C<$chmod> and
spurting C<$content> into it. If C<$chmod> is set, but C<$content> isn't,
spurts an empty string. Automatically deletes the file with C<END> phaser.

=head2  make-temp-path(:$content, :$chmod)

Alias for C<make-temp-file>

=head2 make-temp-dir($chmod?)

    sub make-temp-dir (Int $chmod? --> IO::Path:D)

Creates a semi-randomly named directory in C<$*TMPDIR>, optionally setting
C<$chmod>, and returns an C<IO::Path> pointing to it. Automatically
C<rmdir>s it with C<END> phaser. It's your responsibility to ensure the
directory is empty at that time.

=end pod
