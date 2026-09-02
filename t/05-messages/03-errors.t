use lib <t/packages/Test-Helpers>;
use Test;
use nqp;
use Test::Helpers;

plan 31;

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
todo 'no location of error, yet', 1 if $*VM.name eq 'jvm';
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
    throws-like "\n\nsay \$<\n\n", Exception,
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
    throws-like { 42.categorize    }, Exception, '.categorize()  on Any throws';
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

# https://github.com/rakudo/rakudo/issues/2385
is-run 'role R2385 { multi method r2385(--> Str) { ... } }; class C2385 does R2385 { multi method r2385(--> Int) { 1 } }',
    'Role methods implemented by a class are checked for return type as well as for arguments',
    :err(/ 'Multi method' .+? 'must be implemented' /), :exitcode(so *);

# https://github.com/rakudo/rakudo/issues/2921
is-run 'bleah:(0)', err => { .contains: 'You can\'t adverb' }, :exitcode{.so},
'Absurd adverbing results in a proper error message';

# https://github.com/rakudo/rakudo/issues/4178
is-run 'close $*OUT; say "hi"', err => { .contains: 'closed handle' }, :exitcode{.so},
'An attempt to use a closed handle results in a proper error message';

is-run ｢has $.x; print "ran"｣,
    'attribute declaration in the mainline is a compile time error',
    :err(/'You cannot declare attribute' .*? '$.x'/), :exitcode(1);

is-run ｢has ($.a, $.b); print "ran"｣,
    'signature attribute declaration in the mainline is a compile time error',
    :err(/'You cannot declare attribute' .*? '$.a'/), :exitcode(1);

throws-like ｢my $FILA = 1; say $?FILA｣, X::Undeclared,
    message => *.contains(Q[Did you mean '$FILA']),
    'an unknown compiler variable is reported with suggestions';

# https://irclogs.raku.org/raku-dev/2025-05-12.html#12:13
subtest 'constraint failure on an unpassed optional parameter explains the implicit default' => {
    plan 5;

    throws-like ｢my subset NonEmpty of Str where *.so; sub foo(NonEmpty :$name) { }; foo()｣,
        X::TypeCheck::Binding::Parameter,
        omitted => *.so,
        message => *.contains('was not passed'),
        'an unpassed named parameter with a subset type mentions the implicit default';

    throws-like ｢my subset NonEmpty of Str where *.so; sub foo(NonEmpty $name?) { }; foo()｣,
        X::TypeCheck::Binding::Parameter,
        message => *.contains('was not passed'),
        'an unpassed optional positional parameter with a subset type mentions the implicit default';

    throws-like ｢sub foo(Str :$name where *.so) { }; foo()｣,
        X::TypeCheck::Binding::Parameter,
        message => *.contains('was not passed'),
        'an unpassed named parameter with a where clause mentions the implicit default';

    throws-like ｢my subset NonEmpty of Str where *.so; sub foo(NonEmpty :$name) { }; foo(name => "")｣,
        X::TypeCheck::Binding::Parameter,
        message => { not .contains('was not passed') },
        'a passed value failing the constraint does not claim the parameter was not passed';

    throws-like ｢my subset NonEmpty of Str where *.so; sub foo(NonEmpty :$name = "".lc) { }; foo()｣,
        X::TypeCheck::Binding::Parameter,
        message => { not .contains('was not passed') },
        'an explicit default failing the constraint does not claim the parameter has none';
}

subtest 'a compile time problem shows the source around it' => {
    plan 14;

    throws-like ｢sub f(Int $x) { }; f("str")｣,
        X::TypeCheck::Argument,
        pre  => 'sub f(Int $x) { }; ',
        post => 'f("str")',
        gist => *.contains('------> '),
        'a check time sorry carries the source on either side of the problem';

    throws-like ｢my $a = 1;
sub f(Int $x) { }; f("str");
my $b = 2｣,
        X::TypeCheck::Argument,
        line => 2,
        pre  => 'sub f(Int $x) { }; ',
        post => 'f("str");',
        'the source shown is cut to the line the problem is on';

    throws-like "my \$a = 1;\r\nsub f(Int \$x) \{ }; f(\"str\");\r\nmy \$b = 2",
        X::TypeCheck::Argument,
        line => 2,
        pre  => 'sub f(Int $x) { }; ',
        post => 'f("str");',
        'a source with carriage return line endings is cut to the line too';

    throws-like ｢my $aaaa = 1; my $bbbb = 2; my $cccc = 3; sub f(Int $x) { }; f("str"); my $dddd = 4; my $eeee = 5; my $ffff = 6｣,
        X::TypeCheck::Argument,
        pre  => { .chars == 40 && .ends-with('sub f(Int $x) { }; ') },
        post => { .chars == 40 && .starts-with('f("str"); my $dddd') },
        'each side of the source shown is cut to forty characters';

    if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
        throws-like ｢my $a = 1; $a xx 2 :foo｣,
            X::Syntax::Adverb,
            pre  => 'my $a = 1; ',
            post => '$a xx 2 :foo',
            'the source is split where the node reporting the problem starts';

        throws-like ｢my $a = 1;
$a xx 2 :foo｣,
            X::Syntax::Adverb,
            pre  => '<BOL>',
            'a problem at the start of a line names the line start';

        throws-like ｢my $a = 1; $a xx 2 :foo
｣,
            X::Syntax::Adverb,
            post => '$a xx 2 :foo',
            'the source after the problem stops at the line end';

        throws-like ｢BEGIN die "boom"｣,
            X::Comp::BeginTime,
            pre  => '<BOL>',
            post => 'BEGIN die "boom"',
            gist => *.contains('------> '),
            'a BEGIN time failure carries the source of the BEGIN';

        throws-like ｢my $a = 1; CHECK die "boom"; my $b = 2｣,
            X::Comp::BeginTime,
            pre  => 'my $a = 1; ',
            post => 'CHECK die "boom"; my $b = 2',
            'a CHECK time failure carries the source of the CHECK';

        throws-like ｢my $a = 1; use Nope::Missing; my $b = 2｣,
            X::CompUnit::UnsatisfiedDependency,
            pre  => 'my $a = 1; ',
            post => 'use Nope::Missing; my $b = 2',
            'a failed module load carries the source of the use';

        throws-like ｢my $a = 1; constant x = die "boom"; my $b = 2｣,
            X::Comp::BeginTime,
            pre  => 'my $a = 1; ',
            post => 'constant x = die "boom"; my $b = 2',
            'a constant whose value fails carries the source of the constant';

        throws-like ｢my $a = 1; no worries (die "boom"); my $b = 2｣,
            X::AdHoc,
            pre  => 'my $a = 1; ',
            post => 'no worries (die "boom"); my $b = 2',
            'a pragma argument that fails carries the source of the pragma';

        throws-like ｢my $a = 1; class A:auth(do { die "boom" }) { }; my $b = 2｣,
            X::Comp::BeginTime,
            pre  => 'my $a = 1; class A',
            post => ':auth(do { die "boom" }) { }; my $b = 2',
            'a colonpair value that fails carries the source of the colonpair';

        throws-like ｢my $a = 1; $a xx 2 :foo; $a xx 3 :bar｣,
            X::Comp::Group,
            gist => { .comb('------> ').elems == 2 },
            'each problem in a group shows its own source';
    }
    else {
        skip 'the source split is placed by the RakuAST frontend', 10;
    }
}

# vim: expandtab shiftwidth=4
