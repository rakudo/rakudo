use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 5;

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

# vim: ft=perl6 expandtab sw=4
