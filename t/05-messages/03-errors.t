use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 23;

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

# https://github.com/rakudo/rakudo/issues/1560
subtest 'subsets get named in typecheck errors' => {
    plan 4;
    my subset MeowMix of Int where .so;

    throws-like { -> MeowMix {}("x") },
        X::TypeCheck::Binding::Parameter, :message{.contains: 'MeowMix'},
    'type only, with wrong type given';

    throws-like { -> MeowMix $ where .self {}("x") },
        X::TypeCheck::Binding::Parameter, :message{.contains: 'MeowMix'},
    'type + where, with wrong type given';

    throws-like { -> MeowMix {}(0) },
        X::TypeCheck::Binding::Parameter, :message{.contains: 'MeowMix'},
    'type only, with failing constraint';

    throws-like { -> MeowMix $ where .self {}(0) },
        X::TypeCheck::Binding::Parameter, :message{.contains: 'MeowMix'},
    'type + where, with failing constraint';
}

subtest 'like/unlike failures give useful diagnostics' => {
    plan 2;
    is-run ｢use Test; plan 1; like 42, /43/｣,
        :1exitcode, :out(*), :err{.contains: 'expected a match with'},
    '`like` says it wanted a match, not just "expected"';
    is-run ｢use Test; plan 1; unlike 42, /42/｣,
        :1exitcode, :out(*), :err{.contains: 'expected no match with'},
    '`unlike` says it wanted no match, not just "expected"';
}

# https://github.com/rakudo/rakudo/issues/1644
throws-like ｢Lock.protect: %()｣, X::Multi::NoMatch,
    'Lock.protect with wrong args gives sane error';
throws-like ｢Lock::Async.protect: %()｣, X::Multi::NoMatch,
    'Lock::Async.protect with wrong args gives sane error';

# https://github.com/rakudo/rakudo/issues/1699
throws-like {
    with Proc::Async.new: :out, :!err, $*EXECUTABLE, '-e', '' {
        .bind-stdout: IO::Handle.new;
        .start;
    }
}, Exception, :message{.contains: 'handle not open'},
  'trying to bind Proc::Async to unopened handle gives useful error';

# RT #132238
subtest 'unclosed hash quote index operator <> message' => {
    plan 2;
    throws-like "\n\nsay \$<\n\n", X::Comp::AdHoc,
        'good error message for unclosed <> hash operator',
        gist => all(
            /:i:s<<unable to parse /, /<<find\h+\'\>\'/, /:s<<at line 3 /
        );
    todo 'RT #132238 - remove "expecting any of:"';
    throws-like "say \$<", X::Comp::AdHoc,
        'better and shorter error message for unclosed <> hash operator',
        :gist{ not .match: /:i:s<<expecting any of: / };
}

# RT #122980
throws-like 'Int:erator:$;', X::InvalidTypeSmiley,
    ｢Don't report "missing semicolon" when semicolon present with complicated punctuation.｣,
    :message{ not .match: /:i:s<<missing semicolon/ };


# RT #133107
is-run ｢use IO::Socket::Async::BlahBlahBlah｣, :exitcode(*.so),
    :err{.contains: 'Could not find' & none 'builtin type'},
'non-found module in core namespace is not claimed to be built-in';

# https://github.com/rakudo/rakudo/issues/1848
throws-like ｢
    my class Supercalifragilisticexpialidocious {};
    (my $x := my class {}.new).^set_name: <Supercalifragilisticexpialidocious>;
    -> Supercalifragilisticexpialidocious {}($x)
｣, X::TypeCheck, :message{2 == +.comb: 'Supercalifragilisticexpialidocious'},
    'X::TypeCheck does not prematurely chop off the .perl';

#RT #128646
subtest '.polymod with zero divisor does not reference guts in error' => {
    plan 4;
    throws-like { 1.polymod: 0           }, X::Numeric::DivideByZero,
        gist => /^ [<!after 'CORE.setting.'> . ]+ $/, 'Int';

    throws-like { 1.Rat.polymod: 0       }, X::Numeric::DivideByZero,
        gist => /^ [<!after 'CORE.setting.'> . ]+ $/, 'Real';

    throws-like { 1.polymod: lazy 0,     }, X::Numeric::DivideByZero,
        gist => /^ [<!after 'CORE.setting.'> . ]+ $/, 'Int (lazy)';

    throws-like { 1.Rat.polymod: lazy 0, }, X::Numeric::DivideByZero,
        gist => /^ [<!after 'CORE.setting.'> . ]+ $/, 'Real (lazy)';
}

# RT 126220
throws-like '++.++', X::Multi::NoMatch,
    '++.++ construct does not throw LTA errors';

# RT #128830
throws-like 'while (0){}', X::Syntax::Missing,
    message => /'whitespace' .* 'before curlies' .* 'hash subscript'/,
'lack of whitespace in while (0){} suggests misparse as hash subscript';

# RT #128803
is-run '*...‘WAT’', :err{not .contains: 'SORRY'}, :out(''), :exitcode{.so},
    'runtime time errors do not contain ==SORRY==';

# RT #124219
is-run ｢
    grammar Bug { token term { a }; token TOP { <term> % \n } }
    Bug.parse( 'a' );
｣, :err(/'token TOP { <term>'/), :exitcode{.so},
    '`quantifier with %` error includes the token it appears in';

# RT #125181
is-run 'sub rt125181 returns Str returns Int {}',
    :err{ not $^o.contains: 'Unhandled exception' }, :exitcode{.so},
'using two `returns` traits does not cry about unhandled CONTROl exceptions';

{ # coverage; 2016-09-18
    throws-like { 42.classify      }, Exception, '.classify()    on Any throws';
    throws-like { 42.classify:   * }, Exception, '.classify(*)   on Any throws';
    throws-like { 42.categorize    }, Exception, '.categorize()  on Any throws';
    throws-like { 42.categorize: * }, Exception, '.categorize(*) on Any throws';
}

# vim: ft=perl6 expandtab sw=4
