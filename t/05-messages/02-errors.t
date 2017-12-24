use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 21;

# RT #132295

is-run ｢:2(1)｣, :err{.contains: ｢use 1.base(2) instead｣}, :exitcode(* !== 0),
    ':2(1) suggests using 1.base(2)';

# RT #132291

throws-like { for [:a] X [:b] -> ($i, $j) { } },
    Exception,
    message => / '<anon>' /,
    "anonymous subs get '<anon>' in arity error messages";

todo 'needs better error message';
throws-like {
    sub l { IO::Socket::Async.listen: "localhost", 111390 }
    react whenever l() {
        whenever l() {} # try to listen on already open sock
    }
}, X::AdHoc, message => /'something good'/;

# RT #132283
is-deeply class { has $.bar }.^methods».name.sort, <BUILDALL bar>,
    'auto-generated methods present in .^methods';

# RT #124434
is-run ｢Failure.new(Exception.new); Nil｣, :1exitcode,
    :err{ .contains: "Died with Exception" },
    'Failure.new(Exception.new) does not segfault';

throws-like { (1, 2, 3)[42] = 21 }, X::Assignment::RO,
    :message{ .contains: "List" & none "Str" },
'Trying to assign to immutable List element gives useful error';

# RT #126184
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

# RT #131362
throws-like ｢my $x; $x = 50; 42 = $x｣, X::Assignment::RO,
    :message{.contains: '42'},
'RO assignment indicates value of the thing being assigned into';

# RT #130446
is-run ｢my %h = <a 1 b 2>; enum Bits (%h)｣, :err{
    .contains: 'No values supplied to enum'
             & 'does %h need to be declared constant'
}, 'declaring enum with uninitialized hash warns about it';

{ # RT #125300
    is-run ｢=end MEOWS｣, :err{ /«Pod»/ && .contains: '=begin MEOWS' },
        :exitcode(*),
        'error with `=end FOO` suggests Pod mistake and offers `=begin FOO`';

    is-run ｢=for｣, :err(/«Pod»/), :exitcode(*),
        'error for `=for` suggests it might be a Pod mistake';
}

{ # RT #125596
    is-run ｢say 1 if;｣, :err{
            1 == .comb: 'Whitespace required'
        and 1 == .comb: ｢keyword 'if'｣
    }, :1exitcode, '`say 1 if;` does not repeat error';

    is-run ｢say 1 unless;｣, :err{
            1 == .comb: 'Whitespace required'
        and 1 == .comb: ｢keyword 'unless'｣
    }, :1exitcode, '`say 1 unless;` does not repeat error';
}

# RT #126539
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

#RT #115326
is-run '(:::[])', :err(/"No such symbol ':<>'"/), :1exitcode,
    'no guts spillage with `(:::[])`';

# https://github.com/rakudo/rakudo/issues/1333
is-run 'use Test; cmp-ok 1, "!eqv", 2',
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

# RT #122907
throws-like { sprintf "%d" }, X::Str::Sprintf::Directives::Count,
    :message('Your printf-style directives specify 1 argument, but no '
      ~ 'argument was supplied'),
    'sprintf %d directive with find a corresponding argument throws';

{ # https://github.com/perl6/roast/commit/20fe657466
    my int @arr;
    throws-like { @arr[0] := my $a }, Exception,
        :message('Cannot bind to a natively typed array'),
        'error message when binding to natively typed array';
    throws-like { @arr[0]:delete   }, Exception,
        :message('Cannot delete from a natively typed array'),
        'error message when :deleting from natively typed array';
}

# vim: ft=perl6 expandtab sw=4
