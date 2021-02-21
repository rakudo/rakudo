use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 48;

# https://github.com/Raku/old-issue-tracker/issues/6613

is-run ｢:2(1)｣, :err{.contains: ｢use 1.base(2) instead｣}, :exitcode(* !== 0),
    ':2(1) suggests using 1.base(2)';

# https://github.com/Raku/old-issue-tracker/issues/6609
throws-like { for [:a] X [:b] -> ($i, $j) { } },
    Exception,
    message => / '<anon>' /,
    "anonymous subs get '<anon>' in arity error messages";

throws-like {
    sub l { IO::Socket::Async.listen: "localhost", 111390 }
    react whenever l() {
        whenever l() {} # try to listen on already open sock
    }
}, X::TypeCheck::Binding::Parameter, message => /'type check failed'/;

# https://github.com/Raku/old-issue-tracker/issues/6602
is-deeply class { has $.bar }.^methods».name.sort, <BUILDALL bar>,
    'auto-generated methods present in .^methods';

# https://github.com/Raku/old-issue-tracker/issues/3799
is-run ｢Failure.new(Exception.new); Nil｣, :1exitcode,
    :err{ .contains: "Died with Exception" },
    'Failure.new(Exception.new) does not segfault';

throws-like { (1, 2, 3)[42] = 21 }, X::Assignment::RO,
    :message{ .contains: "List" & none "Str" },
'Trying to assign to immutable List element gives useful error';

# https://github.com/Raku/old-issue-tracker/issues/4591
if $*DISTRO.is-win {
    skip ｢is-run() routine doesn't quite work right on Windows｣;
}
else {
    is-run ｢
        # !! NOTE !! Code's structure is important, to keep correct line number
        my $supply = supply {
            die 'pass' # line 4
        }
        react {  # line 6
            whenever $supply { }
        }
    ｣, :err{.contains: 'pass' & 'line 4' & 'line 6' }, :1exitcode,
        'death in whenevered Supply referenced original location of throw';
}

subtest 'using wrong sigil on var suggests correct variable name' => {
    plan 3;

    throws-like ｢my @foo; $foo[1] = 42｣, X::Undeclared, :message(/'Did you mean' .+ '@foo'/),
        '@array for $array';
    throws-like ｢my %foo; $foo<2> = 42｣, X::Undeclared, :message(/'Did you mean' .+ '%foo'/),
        '%hash for $hash';
    throws-like ｢my @foo; my %foo; $foo<2> = 42｣, X::Undeclared,
      :message(/'Did you mean' .+ [ '@foo' .+ '%foo' | '%foo' .+ '@foo' ]/),
        '@foo and %foo for $foo, when both are declared';
}

# https://github.com/Raku/old-issue-tracker/issues/6267
throws-like ｢my $x; $x = 50; 42 = $x｣, X::Assignment::RO,
    :message{.contains: '42'},
'RO assignment indicates value of the thing being assigned into';

# https://github.com/Raku/old-issue-tracker/issues/5943
is-run ｢my %h = <a 1 b 2>; enum Bits (%h)｣, :err{
    .contains: 'No values supplied to enum'
             & 'does %h need to be declared constant'
}, 'declaring enum with uninitialized hash warns about it';

# https://github.com/Raku/old-issue-tracker/issues/4285
{
    is-run ｢=end MEOWS｣, :err{ /«Pod»/ && .contains: '=begin MEOWS' },
        :exitcode(*),
        'error with `=end FOO` suggests Pod mistake and offers `=begin FOO`';

    is-run ｢=for｣, :err(/«Pod»/), :exitcode(*),
        'error for `=for` suggests it might be a Pod mistake';
}

# https://github.com/Raku/old-issue-tracker/issues/4392
{
    is-run ｢say 1 if;｣, :err{
            1 == .comb: 'Whitespace required'
        and 1 == .comb: ｢keyword 'if'｣
    }, :1exitcode, '`say 1 if;` does not repeat error';

    is-run ｢say 1 unless;｣, :err{
            1 == .comb: 'Whitespace required'
        and 1 == .comb: ｢keyword 'unless'｣
    }, :1exitcode, '`say 1 unless;` does not repeat error';
}

# https://github.com/Raku/old-issue-tracker/issues/4718
if $*DISTRO.is-win {
    skip ｢is-run() routine doesn't quite work right on Windows｣;
}
else {
    is-run ｢
        # We're lookin...
        # for...
        # line number
        class MyInt is Any is Int { } # line 5
    ｣, :err{.contains: ':5' }, :1exitcode,
        'C3 linearization mentions line number';
}

# https://github.com/Raku/old-issue-tracker/issues/2931
is-run '(:::[])', :err(/"No such symbol ':<>'"/), :1exitcode,
    'no guts spillage with `(:::[])`';

# https://github.com/rakudo/rakudo/issues/1333
is-run 'use Test; cmp-ok 1, "!eqv", 2',
    :compiler-args[<-I lib>],
    :out{.starts-with: 'not ok 1'},
    :err{.contains: '!eqv' & 'pass it as a Callable' }, :1exitcode,
    'cmp-ok with Str metaop comparator suggests a working alternative`';

# https://github.com/rakudo/rakudo/pull/1321
throws-like {
    multi ambigu-arg-tester (Int) { say 'here'  }
    multi ambigu-arg-tester (Str) { say 'there' }
    ambigu-arg-tester <42>
}, X::Multi::Ambiguous, :message{ .contains: 'ambigu-arg-tester' & 'IntStr' },
    'an ambiguous call includes the arguments in the error message';


# https://github.com/Raku/old-issue-tracker/issues/3542
# GH #3682
throws-like { sprintf "%d" }, X::Str::Sprintf::Directives::Count,
    :message('Your printf-style directives specify 1 argument, but no '
      ~ "argument was supplied.\nAre you using an interpolated '\$'?"),
    'sprintf %d directive with one directive and no corresponding argument throws';

{ # https://github.com/perl6/roast/commit/20fe657466
    for int, "int", num, "num", str, "str" -> \type, $name {
        my @array := array[type].new;
        throws-like { @array[0] := my $a }, Exception,
            :message("Cannot bind to a native $name array"),
            "error message when binding to native $name array";
        throws-like { @array[0]:delete   }, Exception,
            :message("Cannot delete from a native $name array"),
            "error message when :deleting from native $name array";

        my @shaped := array[type].new(:shape(42));
        throws-like { @shaped[0] := my $a }, Exception,
            :message("Cannot bind to a native $name array"),
            "error message when binding to native $name array";
        throws-like { @shaped[0]:delete   }, Exception,
            :message("Cannot delete from a native $name array"),
            "error message when :deleting from shaped native $name array";
    }
}

# https://github.com/rakudo/rakudo/issues/1346
subtest 'USAGE with subsets/where and variables with quotes' => {
    plan 3;

    sub uhas (\sig, Mu \c, \desc)  {
        is-run ｢sub MAIN (｣ ~ sig ~ ｢) {}｣,
            :err{.contains: c}, :out(*), :exitcode(*), desc
    }

    group-of 3 => 'named params' => {
        uhas ｢UInt :$x!｣,          'UInt', 'mentions subset name';
        uhas ｢Int :$x! where 42｣,  'Int where { ... }',
            'Type + where clauses shown sanely';
        uhas ｢UInt :$x! where 42｣, 'UInt where { ... }',
            'subset + where clauses shown sanely';
    }
    group-of 3 => 'anon positional params' => {
        uhas ｢UInt $｣,          '<UInt>', 'mentions subset name';
        uhas ｢Int $ where 42｣,  'Int where { ... }',
            'where clauses shown sanely';
        uhas ｢UInt $ where 42｣, 'UInt where { ... }',
            'subset + where clauses shown sanely';
    }

    uhas ｢$don't｣, ｢<don't>｣,
        'variable name does not get special quote treatment';
}

if $*DISTRO.is-win {
    skip ｢is-run() routine doesn't quite work right on Windows｣;
}
else { subtest ':bundling and negation/explicit arguments'=> {
    plan 6;

    my $allows-bundling = q:to/EOF/;
        my %*SUB-MAIN-OPTS = :bundling;
        sub MAIN($pos, :$a, :$b, :$c) {}
    EOF

    is-run $allows-bundling, :err{.contains: 'combine bundling'}, :exitcode(1), :args<-abc=foo bar>,
        'cannot combine bundling with explicit arguments';
    is-run $allows-bundling, :err{.contains: 'combine bundling'}, :exitcode(1), :args<-abc='' bar>,
        'cannot combine bundling with explicit arguments, even the empty string';
    is-run $allows-bundling, :err{.contains: 'combine bundling'}, :exitcode(1), :args<-abc= bar>,
        'cannot combine bundling with explicit arguments, even a nil argument';
    is-run $allows-bundling, :exitcode(0), :args<-a=foo bar>,
        'can pass explicit argument to a single option, even with bundling enabled';
    is-run $allows-bundling, :err{.contains: 'combine bundling'}, :exitcode(1), :args<-/abc bar>,
        'cannot combine bundling with negation';
    is-run $allows-bundling, :exitcode(0), :args<-/a bar>,
        'can negate single option, even with bundling enabled';
}}


# https://github.com/Raku/old-issue-tracker/issues/5282
{
    throws-like { 'foo'.substr(5) }, X::OutOfRange,
        :message(/'Start argument to substr' .+ 'should be in 0..3' .+ '*-5'/);
    throws-like { ''.substr(1000) }, X::OutOfRange,
        :message(/'should be in 0..0' .+ '*-1000'/);
}

# https://github.com/Raku/old-issue-tracker/issues/4653
for ThreadPoolScheduler.new, CurrentThreadScheduler -> $*SCHEDULER {
    is-run q[Supply.interval(1).tap(-> { say 'hi' }); sleep 3;],
    :1exitcode, :err(/
        'Unhandled exception in code scheduled on thread' .+
        'Too many positionals' .+ 'expected 0 arguments but got 1'
    /), '.tap block with incorrect signature must fail';
}

# https://github.com/Raku/old-issue-tracker/issues/5290
is-run ｢133742.print｣, :compiler-args[<--rxtrace>], :out{ .ends-with: 133742 },
    '--rxtrace does not crash';

# https://github.com/rakudo/rakudo/issues/1336
throws-like ｢
    multi z (@a, Int, :$x where 1) {}
    multi z (@a, Str, :$x where 1) {}
    my @a = 1..200; z(@a, <1>, :x[1..200])
｣, X::Multi::NoMatch, :message{ .chars < 200 },
    'X::Multi::NoMatch does not dump entire contents of variables';

# https://github.com/Raku/old-issue-tracker/issues/6633
throws-like ｢Set.new(1..300)<42> = 42｣,
    X::Assignment::RO, :message{ .chars < 100 },
    'X::Assignment::RO does not dump entire contents of variables';

# https://github.com/Raku/old-issue-tracker/issues/4949
subtest 'cannot use Int type object as an operand' => {
    plan 14;

    throws-like ｢(1/1)+Int｣,
        X::Numeric::Uninitialized,
        'A Rational instance cannot be added by an Int type object';
    throws-like ｢Int+(1/1)｣,
        X::Numeric::Uninitialized,
        'An Int type object cannot be added by a Rational instance';
    throws-like ｢(1/1)-Int｣,
        X::Numeric::Uninitialized,
        'A Rational instance cannot be subtracted by an Int type object';
    throws-like ｢Int-(1/1)｣,
        X::Numeric::Uninitialized,
        'An Int type object cannot be subtracted by a Rational instance';
    throws-like ｢(1/1)*Int｣,
        X::Numeric::Uninitialized,
        'A Rational instance cannot be multiplied by an Int type object';
    throws-like ｢Int*(1/1)｣,
        X::Numeric::Uninitialized,
        'An Int type object cannot be multiplied by a Rational instance';
    throws-like ｢(1/1)/Int｣,
        X::Numeric::Uninitialized,
        'A Rational instance cannot be divided by an Int type object';
    throws-like ｢Int/(1/1)｣,
        X::Numeric::Uninitialized,
        'An Int type object cannot be divided by a Rational instance';
    throws-like ｢Int/Int｣,
        X::Numeric::Uninitialized,
        'An Int type object cannot be divided by an Int type object';
    throws-like ｢Int/1｣,
        X::Numeric::Uninitialized,
        'An Int type object cannot be divided by an Int instance';
    throws-like ｢1/Int｣,
        X::Numeric::Uninitialized,
        'An Int instance cannot be divided by an Int type object';
    throws-like ｢(1/1)%Int｣,
        X::Numeric::Uninitialized,
        'A Rational instance modulo an Int type object is incalculable';
    throws-like ｢Int%(1/1)｣,
        X::Numeric::Uninitialized,
        'An Int type object modulo a Rational instance is incalculable';
    throws-like ｢(1/1)**Int｣,
        X::Numeric::Uninitialized,
        'A Rational instance cannot be powered by an Int type object';
}

# https://github.com/rakudo/rakudo/issues/1364
throws-like ｢sub meows;｣, X::UnitScope::Invalid, :message(/
    "placed a semicolon after routine's definition"
/), 'unit-scoped sub def mentions potential unwanted semicolon';

# https://github.com/rakudo/rakudo/issues/1305
throws-like { my $r = 1..5; $r[42] = 21 }, X::Assignment::RO,
    :message{ .contains: 'Range' & none 'Str', '(Nil)' },
    'Trying to assign to immutable Range element gives useful error';

# The warning for `*+*` in void context is handled by the optimizer so
# if we turn off the optimizer, we'd get clean STDERR/STDOUT, which is what
# the this test checks.
is-run 'EVAL "*+*"', :compiler-args[<--optimize=off>],
    'optimizer flag gets propagated to EVAL';

# https://github.com/Raku/old-issue-tracker/issues/4760
throws-like ｢use 6.0;｣, X::Undeclared::Symbols, :message{
    .contains: 'use "v" prefix for pragma (e.g., "use v6;", "use v6.c;")'
}, 'suggests to use "use v6;" or "use v6.c;" when "use 6.0" is called';

# https://github.com/Raku/old-issue-tracker/issues/6567
throws-like ｢need 6.0;｣, X::Undeclared::Symbols, :message{
    .contains: 'use "v" prefix and "use" for pragma (e.g., "use v6;", '
               ~ '"use v6.c;")'
}, 'suggests to use "use v6;" or "use v6.c;" when "need 6.0" is called';

throws-like ｢need v6.0;｣, Exception, :message{
    .contains: 'In case of using pragma, use "use" instead (e.g., '
                ~ '"use v6;", "use v6.c;").'
}, 'suggests to use "use v6;" or "use v6.c;" when "need v6.0" is called';

# https://github.com/Raku/old-issue-tracker/issues/4839
throws-like ｢^42  .^methods.say｣, X::Syntax::Malformed,
    :message{ .contains: 'only basic method calls that exclusively use a dot can be detached' },
    'detached non-alpha method says what the problem is';

#### THIS FILE ALREADY LOTS OF TESTS ADD NEW TESTS TO THE NEXT error.t FILE

# vim: expandtab shiftwidth=4
