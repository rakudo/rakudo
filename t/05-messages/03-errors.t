use lib <t/packages/Test-Helpers t/packages/05-messages/lib>;
use Test;
use Test::Helpers;

plan 32;

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

subtest 'a store of a value a variable can never accept is reported at compile time' => {
    plan 53;

    throws-like ｢my Str @a = 1,2,3｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Int (1)' & 'type Str'),
        'a comma list of literals names the first one the element type rejects';

    throws-like ｢my Str @a = "a", 2｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Int (2)'),
        'a literal later in the list is reported once the earlier ones are accepted';

    throws-like ｢my Str @a = (1,2,3)｣,
        X::Syntax::Number::LiteralType,
        'grouping parentheses around the list are looked through';

    throws-like ｢my Str @a = [1,2,3]｣,
        X::Syntax::Number::LiteralType,
        'an array composer around the list is looked through';

    throws-like ｢my Str @a = (1; 2)｣,
        X::Syntax::Number::LiteralType,
        'each statement of a semicolon list stores an element';

    throws-like ｢my Str @a = 1｣,
        X::Syntax::Number::LiteralType,
        'a single literal is reported the same as a list of them';

    throws-like ｢my Str @a; @a = 1,2,3｣,
        X::Syntax::Number::LiteralType,
        'a list assignment is reported the same as a list declaration';

    throws-like ｢my Str $x = 1｣,
        X::Syntax::Number::LiteralType,
        'an item declaration reports a value its type can never accept';

    throws-like ｢my Str $x; $x = 1｣,
        X::Syntax::Number::LiteralType,
        'an item assignment is reported the same as an item declaration';

    throws-like ｢state Str $x = 1｣,
        X::Syntax::Number::LiteralType,
        'a state declaration is judged the same as a my declaration';

    throws-like ｢my Int $x = ("foo")｣,
        X::Syntax::Number::LiteralType,
        'grouping parentheses around an item value are looked through';

    throws-like ｢my Int @a = "foo"｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Str ("foo")'),
        'a string literal is reported the same as a number';

    throws-like ｢my Str @a = v1.2｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Version'),
        'a version literal is reported the same as a number';

    throws-like ｢constant Half = 1/2; my Str $x = Half｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Rat'),
        'a constant stands for a value the compiler already knows';

    throws-like ｢my Bool @b = 1,2｣,
        X::Syntax::Number::LiteralType,
        'an enumeration element type reports a value it can never accept';

    throws-like ｢my num @a = 1,2｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('native variable'),
        'a native element type reports a value it can never unbox';

    throws-like ｢my int $x = "foo"｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('native variable'),
        'a native item type reports a value it can never unbox';

    throws-like ｢my uint8 $x = "foo"｣,
        X::Syntax::Number::LiteralType,
        message => { .contains('of type uint8') && .contains('Int("foo")') },
        'a native is named as declared, and coerced to the type it unboxes from';

    throws-like ｢my num $x = "foo"｣,
        X::Syntax::Number::LiteralType,
        message => { .contains('type num') && .contains('Num("foo")') },
        'a native floating point unboxes from a type of its own family';

    throws-like ｢my num $x = 1｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('write the value as 1e0'),
        'a native is offered the value written as the type it unboxes from';

    throws-like ｢my str $x = 1｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('type str'),
        'a native string reports a number it can never unbox';

    throws-like ｢my Nil $x = 1｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('type Nil'),
        'a Nil type is named as itself rather than reset to a default';

    throws-like ｢my Bool $x = <abc>｣,
        X::Syntax::Number::LiteralType,
        'a single word is the one value it stands for';

    throws-like ｢my Int @a = <a b>｣,
        X::Syntax::Number::LiteralType,
        'a word list is judged word by word';

    throws-like ｢my Int @a = qw{a b}｣,
        X::Syntax::Number::LiteralType,
        'a qw word list is judged like the comma list it stands for';

    throws-like ｢my Bool @a = <1 2 3>｣,
        X::Syntax::Number::LiteralType,
        'a word list of numbers is judged as the allomorphs it builds';

    throws-like ｢enum Colour <white>; my Colour $c = "nope"｣,
        X::Syntax::Number::LiteralType,
        'a user defined enumeration reports a value it can never accept';

    throws-like ｢sub f(Str $x is copy) { $x = 43 }｣,
        X::Syntax::Number::LiteralType,
        'a copied item parameter is given a container of the type it names';

    throws-like ｢sub f(Str $x is rw) { $x = 43 }｣,
        X::Syntax::Number::LiteralType,
        'a parameter storing a value its own type rejects is reported';

    throws-like ｢my (Int $a); $a = "str"｣,
        X::Syntax::Number::LiteralType,
        'a variable declared in a declarator signature is reported too';

    throws-like ｢my Int $x = (("foo"))｣,
        X::Syntax::Number::LiteralType,
        'nested grouping parentheses are looked through';

    throws-like ｢sub g($q) { my Int $a; $a := $q }; my Int $a; $a = "x"｣,
        X::Syntax::Number::LiteralType,
        'a bind to one variable leaves another of the same name in another scope alone';

    throws-like ｢my Str $x = $?LINE｣,
        X::Syntax::Number::LiteralType,
        'a compile time variable is the value the compiler reads for it';

    throws-like ｢constant $half = 0.5; my Str $x = $half｣,
        X::Syntax::Number::LiteralType,
        'a constant written with a sigil is the value it stands for';

    throws-like ｢sub f(Str $x is copy where { True }) { $x = 42 }｣,
        X::Syntax::Number::LiteralType,
        'a where clause narrows what a type accepts rather than widening it';

    throws-like ｢my Int $x = "foo"｣,
        X::Syntax::Number::LiteralType,
        got => 'foo', expected => Int, symbol => '$x',
        'the report answers the value, the type and the variable it was going to';

    throws-like ｢my Int $x = 4.2｣,
        X::Syntax::Number::LiteralType,
        message => { .contains('of type Real') && .contains('write the value as 4') },
        'a type both the value and the variable fit is named as the one to declare';

    throws-like ｢my Complex $x = 1｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('of type Numeric'),
        'a value and a variable sharing only Numeric are told to declare that';

    throws-like ｢my IO::Path $x = 1.5｣,
        X::Syntax::Number::LiteralType,
        message => { not .contains('coerce') },
        'a type the value has no coercion method for is not advised as one';

    throws-like ｢my %h; my $v = 1; %h<k> := $v; my Str @a = 1,2,3｣,
        X::Syntax::Number::LiteralType,
        'a bind into an aggregate leaves the containers declarations made alone';

    throws-like ｢my Str @a[2] = 1,2｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Int (1)'),
        'a shaped declaration is judged like the unshaped one it binds';

    throws-like ｢my Int $x = pi｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Num'),
        'a constant the setting declares is the value it stands for';

    throws-like ｢my Str $x = BEGIN 1｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Int (1)'),
        'a BEGIN block has run by now, so what it produced is a value';

    throws-like ｢my Str @a = ("a"; 2)｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Int (2)'),
        'a later statement of a semicolon list is judged once the earlier ones fit';

    throws-like ｢my Int $x = "foo"｣,
        X::Syntax::Number::LiteralType,
        message => { not .contains('write the value as') },
        'a value the coercion cannot convert is not offered back as a spelling';

    if %*ENV<RAKUDO_RAKUAST> {
        throws-like ｢my Str @a = @a, 1｣,
            X::Comp,
            message => { .comb('Cannot assign a literal') == 1 },
            'a store held back for a declaration already complaining is listed exactly once';
    }
    else {
        throws-like ｢my Str @a = @a, 1｣,
            X::Syntax::Variable::Initializer,
            'a declaration complaining about its initializer reports before any store is judged';
    }

    throws-like ｢use TypedExports; my Str $x = Answer｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Int (42)'),
        'a constant read out of another comp unit is a value the compiler holds';

    throws-like ｢my Int $x = *｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Whatever (*)'),
        'a whatever is the singleton it stands for';

    throws-like ｢my Int $x = **｣,
        X::Syntax::Number::LiteralType,
        'a hyper whatever stands for a singleton of its own';

    throws-like ｢my Str $x = constant Q1 = 1｣,
        X::Syntax::Number::LiteralType,
        message => *.contains('Int (1)'),
        'a constant declaration evaluates to what it was declared as';

    throws-like ｢my Str $x = 1; OUR::<$y> := 5｣,
        X::Syntax::Number::LiteralType,
        'a bind through the package stash of the current package reaches no lexical';

    throws-like ｢my Str $x = 1; GLOBAL::<$y> := 5｣,
        X::Syntax::Number::LiteralType,
        'a bind through the global stash reaches no lexical either';

    throws-like ｢my Str $x = 1; $OUR::y := 5｣,
        X::Syntax::Number::LiteralType,
        'a sigil qualified bind through a package stash reaches no lexical';
}

subtest 'a store the compiler cannot judge is left to the run time check' => {
    plan 53;

    # The compile time report is a subclass of the run time one, so ruling out
    # its wording is what says the check was left where it was.
    my &at-run-time = { not .contains('Cannot assign a literal') };

    throws-like ｢my $w = "a"; my Int @a = «$w b»｣,
        X::TypeCheck, :message(&at-run-time),
        'an interpolating word list settles at run time which words it produces';

    throws-like ｢my Str @a; @a.STORE(1)｣,
        X::TypeCheck, :message(&at-run-time),
        'a written out call to STORE is one the declaration does not speak for';

    throws-like ｢my Str $ = 1｣,
        X::TypeCheck, :message(&at-run-time),
        'an anonymous variable is known here only by a name the compiler made up';

    throws-like ｢my @l = 1; my Int $x; $x = "s"; $x := @l[0]｣,
        X::TypeCheck, :message(&at-run-time),
        'a bind stands the check down for a store written before it';

    lives-ok { EVAL ｢sub f(Int $x is copy) { my $c = 1; $x := $c; $x = "b" }; f(1)｣ },
        'a rebound parameter meets the container the bind gave it';

    lives-ok { EVAL ｢my Str $s; sub f() { $s = 42 }; my $c = "x"; $s := $c; f()｣ },
        'a store in a routine can run after a bind written later in the file';

    throws-like ｢my Int $a; sub g($q) { $a := $q }; $a = "x"｣,
        X::TypeCheck, :message(&at-run-time),
        'a bind reaches a declaration made outside the scope the bind is written in';

    lives-ok { EVAL ｢sub f(Int $x is copy) { $x = "b"; my $c = 1; $x := $c }｣ },
        'a parameter bound after the store is excused like any other variable';

    throws-like ｢my Str $s; $s = 42; BEGIN { EVAL q|1| }; my $c = "x"; $s := $c｣,
        X::TypeCheck, :message(&at-run-time),
        'a comp unit compiled part way through does not judge the stores held back';

    throws-like ｢my Str $x = <a b>｣,
        X::TypeCheck, :message(&at-run-time),
        'a word list stored whole is spread across stores the element type sees';

    lives-ok { EVAL ｢use TypedExports; $count = "x"; my Str $a = $count｣ },
        'an imported variable is held as its container, not as a value it once had';

    lives-ok { EVAL ｢my Str $Foo::a = 1｣ },
        'a name in a package makes no container the declared type speaks for';

    lives-ok { EVAL ｢my Int $x; my $y = 3; MY::<$x> := $y; $x = "abc"｣ },
        'a bind through a pseudo package reaches a variable this cannot name';

    lives-ok { EVAL ｢my Int $x; my $y = 3; my $n = q[$x]; MY::{$n} := $y; $x = "abc"｣ },
        'a pseudo package bind naming its target at run time stands the check down';

    lives-ok { EVAL ｢my Int $x; sub f() { my $y = 3; OUTER::<$x> := $y }; f(); $x = "abc"｣ },
        'a pseudo package naming an enclosing scope reaches a lexical just as well';

    lives-ok { EVAL ｢my Int $x; my $y = 3; $MY::x := $y; $x = "abc"｣ },
        'a sigil qualified pseudo package bind reaches a lexical the same way';

    lives-ok { EVAL ｢my Str @a; @MY::a := [1]; @a = 1, 2｣ },
        'a sigil qualified pseudo package bind reaches a list variable too';

    throws-like ｢my Str $x = &say｣,
        X::TypeCheck, :message(&at-run-time),
        'a routine is reached by a reference to its declaration, not held as a value';

    throws-like ｢my Int $x = * + 1｣,
        X::TypeCheck, :message(&at-run-time),
        'a whatever taking part in an expression builds a closure at run time';

    lives-ok { EVAL ｢my Int @a = <1 2 3>｣ },
        'a word list of numbers builds allomorphs an Int array accepts';

    lives-ok { EVAL ｢my Str() @a = 1,2,3｣ },
        'a coercion converts what it is given';

    throws-like ｢subset S of Str; my S @a = 1,2｣,
        X::TypeCheck, :message(&at-run-time),
        'a subset is left alone, since checking one can have side effects';

    throws-like ｢my Int:D $x = "foo"｣,
        X::TypeCheck, :message(&at-run-time),
        'a definite type is nominalizable rather than nominal';

    throws-like ｢role R { }; my R $x = 1｣,
        X::TypeCheck, :message(&at-run-time),
        'a role accepts by what does it rather than by type check';

    throws-like ｢sub f(::T $t) { my T $x = "foo" }; f(1)｣,
        X::TypeCheck, :message(&at-run-time),
        'a generic is only settled later';

    throws-like ｢my Date $x = 1｣,
        X::TypeCheck, :message(&at-run-time),
        'a type that is not Cool has no coercion advice to give';

    throws-like ｢constant Only42 = 42; my Only42 $x = 43｣,
        X::TypeCheck, :message(&at-run-time),
        'a value used as a type has no type of its own to name';

    throws-like ｢my Int %h = 4.2｣,
        X::Hash::Store::OddNumber,
        'a hash takes a flat list of keys and values';

    throws-like ｢my Str @a := 3｣,
        X::TypeCheck::Binding,
        'a bind is checked whole against the bind constraint';

    throws-like ｢sub f(Str $x) { $x = 43 }; f("a")｣,
        X::AdHoc, :message(*.contains('immutable value')),
        'a read only parameter reports the assignment rather than the value';

    lives-ok { EVAL ｢my Int @a = 1, Empty, 3｣ },
        'a value that flattens contributes its elements rather than itself';

    lives-ok { EVAL ｢my Str @a = (1 if 0)｣ },
        'a condition modifier can leave the list without that element';

    lives-ok { EVAL ｢my Str @a = (1 for ^0)｣ },
        'a loop modifier yields a list rather than the element written';

    lives-ok { EVAL ｢my Int @a = (if 1 { 2 })｣ },
        'a statement that is not an expression stores nothing this can name';

    lives-ok { EVAL ｢my Array @x = [1,2],[3,4]｣ },
        'an array composer inside a list is the one array it builds';

    lives-ok { EVAL ｢my Str %h = 1, "a"｣ },
        'a hash key is not a value the element type sees';

    lives-ok { EVAL ｢my @l = 1; my Int $x := @l[0]; $x = "s"｣ },
        'a bind replaces the container the declaration made';

    lives-ok { EVAL ｢my Str $x;
                       $x := Proxy.new(FETCH => -> $ { "a" }, STORE => -> $, $v { });
                       $x = 42｣ },
        'a bind to a proxy leaves the store to whatever the proxy does with it';

    lives-ok { EVAL ｢my List @a = ((1,2); (3,4))｣ },
        'each statement of a semicolon list is one element, taken no further apart';

    lives-ok { EVAL ｢my class Coercing is Array {
                         method STORE(\values, :$INITIALIZE) {
                             self.Array::STORE(values.list.map(*.Int))
                         } }
                       my Int @a is Coercing = "5","6"｣ },
        'a container named by the declaration brings its own STORE';

    lives-ok { EVAL ｢subset Nonempty of Str where *.chars;
                       sub f(Nonempty $x is copy) { $x = 1 }｣ },
        'a parameter type narrowed by a subset is recorded already widened';

    lives-ok { EVAL ｢sub f(Str:D $x is copy) { $x = 1 }｣ },
        'a parameter type narrowed by definiteness is recorded already widened';

    lives-ok { EVAL ｢sub f(Str @a is copy) { @a = 1,2 }｣ },
        'a copied list parameter is given a container without the element type';

    lives-ok { EVAL ｢my List $x = (1; 2)｣ },
        'an item variable stores what a semicolon list builds, not its statements';

    throws-like ｢class Holder { has Str $.y is rw }; Holder.new.y = 1｣,
        X::TypeCheck::Assignment, :message(&at-run-time),
        'an assignment to an attribute keeps its run time check';

    lives-ok { EVAL ｢class Keeper { has Str $!y; method m() { $!y = 1 } }｣ },
        'a store to an attribute in a method body is left to the run time check';

    lives-ok { EVAL ｢my Str $x = Nil｣ },
        'Nil resets a container to its default rather than storing a value';

    lives-ok { EVAL ｢my Int @a = 1, Nil｣ },
        'Nil in a list resets that element rather than storing a value';

    throws-like ｢my Str $x = 1 + 1｣,
        X::TypeCheck, :message(&at-run-time),
        'an expression waiting to be folded is not a value yet';

    throws-like ｢class C { has Str $.x = 42 }｣,
        X::Comp, :message(*.contains('default value')),
        'an attribute default is reported against the attribute';

    throws-like ｢use experimental :will-complain;
                 my Str $s will complain { "gimme a Str, not {.^name}" }; $s = 1｣,
        X::TypeCheck::Assignment,
        message => { .contains('gimme a Str, not Int') && at-run-time($_) },
        'a complaint on the variable is written by the run time check';

    throws-like ｢use experimental :will-complain;
                 my class Wanted is Cool will complain { "gimme a Wanted, not {.^name}" } { };
                 my Wanted $x = 42｣,
        X::TypeCheck::Assignment,
        message => { .contains('gimme a Wanted, not Int') and at-run-time($_) },
        'a complaint on the type is written by the run time check too';

    lives-ok { EVAL ｢use TypedExports; $count = 43.5; @names = 1, 2｣ },
        'an imported symbol answers the type of a value, not of its container';
}

# vim: expandtab shiftwidth=4
