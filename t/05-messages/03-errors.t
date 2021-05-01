use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 25;

subtest '.map does not explode in optimizer' => {
    plan 3;
    throws-like ｢^4 .map: {}｣, X::Cannot::Map, 'Hash';
    throws-like ｢^4 .map: 42｣, X::Cannot::Map, 'Int';

    sub foo ($x) { $x+2};
    is-deeply ^4 .map(&foo), (2, 3, 4, 5).Seq, 'subroutine';
}

throws-like ｢(lazy <a b c>).nodemap: {;}｣, X::Cannot::Lazy, :action<nodemap>,
  'nodemap mentions right action when throwing on lazies';

# https://github.com/rakudo/rakudo/issues/1314
throws-like ｢'x'.substr: /x/, 'x'｣, Exception,
            message => /｢did you mean 'subst'｣/,
            'using substr instead of subst';

# https://github.com/Raku/old-issue-tracker/issues/6672
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
        :1exitcode, :compiler-args[<-I lib>], :out(*), :err{.contains: 'expected a match with'},
    '`like` says it wanted a match, not just "expected"';
    is-run ｢use Test; plan 1; unlike 42, /42/｣,
        :1exitcode, :compiler-args[<-I lib>], :out(*), :err{.contains: 'expected no match with'},
    '`unlike` says it wanted no match, not just "expected"';
}

# https://github.com/rakudo/rakudo/issues/1699
throws-like {
    with Proc::Async.new: :out, :!err, $*EXECUTABLE, '-e', '' {
        .bind-stdout: IO::Handle.new;
        .start;
    }
}, Exception, :message{.contains: 'handle not open'},
  'trying to bind Proc::Async to unopened handle gives useful error';

# https://github.com/Raku/old-issue-tracker/issues/6580
subtest 'unclosed hash quote index operator <> message' => {
    plan 2;
    throws-like "\n\nsay \$<\n\n", X::Comp::AdHoc,
        'good error message for unclosed <> hash operator',
        gist => all(
            /:i:s<<unable to parse /, /<<find\h+\'\>\'/, /:s<<at line 3 /
        );
    todo 'remove "expecting any of:"';
    throws-like "say \$<", X::Comp::AdHoc,
        'better and shorter error message for unclosed <> hash operator',
        :gist{ not .match: /:i:s<<expecting any of: / };
}

# https://github.com/Raku/old-issue-tracker/issues/3553
throws-like 'Int:erator:$;', X::InvalidTypeSmiley,
    ｢Don't report "missing semicolon" when semicolon present with complicated punctuation.｣,
    :message{ not .match: /:i:s<<missing semicolon/ };


# https://github.com/Raku/old-issue-tracker/issues/6683
is-run ｢use IO::Socket::Async::BlahBlahBlah｣, :exitcode(*.so),
    :err{.contains: 'Could not find' & none 'builtin type'},
'non-found module in core namespace is not claimed to be built-in';

# https://github.com/rakudo/rakudo/issues/1848
throws-like ｢
    my class Supercalifragilisticexpialidocious {};
    (my $x := my class {}.new).^set_name: <Supercalifragilisticexpialidocious>;
    -> Supercalifragilisticexpialidocious {}($x)
｣, X::TypeCheck, :message{2 == +.comb: 'Supercalifragilisticexpialidocious'},
    'X::TypeCheck does not prematurely chop off the .raku';

# https://github.com/Raku/old-issue-tracker/issues/5458
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

# https://github.com/Raku/old-issue-tracker/issues/4607
throws-like '++.++', X::Multi::NoMatch,
    '++.++ construct does not throw LTA errors';

# https://github.com/Raku/old-issue-tracker/issues/5526
throws-like 'while (0){}', X::Syntax::Missing,
    message => /'whitespace' .* 'before curlies' .* 'hash subscript'/,
'lack of whitespace in while (0){} suggests misparse as hash subscript';

# https://github.com/Raku/old-issue-tracker/issues/5510
is-run '*...‘WAT’', :err{not .contains: 'SORRY'}, :out(''), :exitcode{.so},
    'runtime time errors do not contain ==SORRY==';

# https://github.com/Raku/old-issue-tracker/issues/3766
is-run ｢
    grammar Bug { token term { a }; token TOP { <term> % \n } }
    Bug.parse( 'a' );
｣, :err(/'token TOP { <term>'/), :exitcode{.so},
    '`quantifier with %` error includes the token it appears in';

# https://github.com/Raku/old-issue-tracker/issues/4242
is-run 'sub rt125181 returns Str returns Int {}',
    :err{ not $^o.contains: 'Unhandled exception' }, :exitcode{.so},
'using two `returns` traits does not cry about unhandled CONTROl exceptions';

{ # coverage; 2016-09-18
    throws-like { 42.classify      }, Exception, '.classify()    on Any throws';
    throws-like { 42.classify:   * }, Exception, '.classify(*)   on Any throws';
    throws-like { 42.categorize    }, Exception, '.categorize()  on Any throws';
    throws-like { 42.categorize: * }, Exception, '.categorize(*) on Any throws';
}

# https://github.com/rakudo/rakudo/issues/2110
subtest 'numeric backslash errors do not get accompanied by confusing others' => {
    plan 3;
    my &err = {.contains: 'backslash sequence' & none 'quantifies nothing' }
    is-run ｢"a" ~~ /(a)\1+$/｣, :&err, :exitcode, 'regex';
    is-run ｢"\1"｣,             :&err, :exitcode, 'double quotes';
    is-run ｢Q:qq:cc/\1/｣,      :&err, :exitcode, ':qq:cc quoter';
}

# https://github.com/Raku/old-issue-tracker/issues/5739
if $*DISTRO.is-win {
    skip ｢is-run() routine doesn't quite work right on Windows｣;
}
else {
    is-run "my \$x = q:to/END/;\ny\n END", :err{ not .contains('Actions.nqp') },
        'heredoc trimming warnings do not reference guts';
}

# https://github.com/rakudo/rakudo/issues/1813
cmp-ok X::OutOfRange.new(
    :what<a range>, :got(0..3000), :range(1..3000)
).message.chars, '<', 150, 'X::OutOfRange does not stringify given Ranges';

# https://github.com/rakudo/rakudo/issues/2320
is-run 'class { method z { $^a } }', :err{ my @lines = $^msg.lines; @lines.grep({ !/'⏏'/ && .contains: '$^a' }) }, :exitcode{.so},
'Use placeholder variables in a method should yield a useful error message';

# vim: expandtab shiftwidth=4
