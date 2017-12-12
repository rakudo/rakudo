use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 9;

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
is-run ｢
    # !!! NOTE !!! Code's structure is important, to keep correct line number
    my $supply = supply {
        die 'pass' # line 4
    }
    react {  # line 6
        whenever $supply { }
    }
｣, :err{.contains: 'pass' & 'line 4' & 'line 6' }, :1exitcode,
    'death in whenevered Supply referenced original location of throw';

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

# vim: ft=perl6 expandtab sw=4
