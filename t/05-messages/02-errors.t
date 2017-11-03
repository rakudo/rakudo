use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 2;

# RT #132295

is-run ｢:2(1)｣, :err{.contains: ｢use 1.base(2) instead｣}, :status(* !== 0),
    ':2(1) suggests using 1.base(2)';

# RT #132291

throws-like { for [:a] X [:b] -> ($i, $j) { } },
    Exception,
    message => / '<anon>' /,
    "anonymous subs get '<anon>' in arity error messages";

# vim: ft=perl6 expandtab sw=4
