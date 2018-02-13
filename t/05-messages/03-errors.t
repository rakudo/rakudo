use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 4;

subtest '.map does not explode in optimizer' => {
    plan 3;
    throws-like ｢^4 .map: {}｣, Exception,
        :message{.contains: 'Cannot map a Range to a Hash.'}, 'hash';
    throws-like ｢^4 .map: 42｣, X::Multi::NoMatch, 'Int';

    sub foo ($x) { $x+2};
    is-deeply ^4 .map(&foo), (2, 3, 4, 5).Seq, 'subroutine';
}

throws-like ｢(lazy <a b c>).nodemap: {;}｣, X::Cannot::Lazy, :action<nodemap>,
  'nodemap mentions right action when throwing on lazies';

# GH#1314
throws-like ｢'x'.substr: /x/, 'x'｣, Exception,
            message => /｢did you mean 'subst'｣/,
            'using substr instead of subst';

# RT #132846
throws-like ｢sprintf "%d", class Foo {}.new｣,
    X::Str::Sprintf::Directives::BadType, :gist(/«line\s+\d+$$/),
'errors from sprintf include location of error';

# vim: ft=perl6 expandtab sw=4
