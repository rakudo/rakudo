use lib <t/packages/Test-Helpers>;
use Test::Helpers;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 26;

# A `my` declaration whose every access stays inside its declaring frame
# is emitted as a frame-local register slot instead of a by-name lexical.
# A static sentinel lexical remains under the declared name, so by-name
# access from another frame still finds a symbol and fails over the same
# way as before. A declaration that escapes, or that a construct can
# reach by name at runtime, keeps its ordinary lexical declaration. Both
# frontends lower, so these assertions hold for either.

sub qast-var-decl (Mu $qast, Str:D $name, Str:D $decl, &value-ok? --> Bool:D) {
    if nqp::istype($qast, QAST::Var) && $qast.name eq $name && $qast.decl eq $decl
      && (!&value-ok || value-ok($qast.value)) {
        return True;
    }
    if qast-descendable $qast {
        for $qast.list {
            qast-var-decl($_, $name, $decl, &value-ok) and return True;
        }
    }
    False
}

sub is-sentinel (Mu $value --> Bool:D) {
    so nqp::eqaddr(nqp::decont($value), Rakudo::Internals::LoweredAwayLexical)
}

# The sentinel that replaces a lowered declaration's by-name symbol.
sub lowered-away (Mu $qast, Str:D $name --> Bool:D) {
    qast-var-decl($qast, $name, 'static', &is-sentinel)
}

# A declaration whose container stays under its name, whichever decl
# carries it.
sub kept-lexical (Mu $qast, Str:D $name --> Bool:D) {
    qast-var-decl($qast, $name, 'contvar')
      or qast-var-decl($qast, $name, 'static', -> Mu $value { !is-sentinel($value) })
}

qast-is 'my $x = 1; say $x', :full, -> \v {
    lowered-away(v, '$x') and not kept-lexical(v, '$x')
}, 'a non-escaping scalar is lowered to a local';

qast-is 'my $x := [1,2]; say $x.elems', :full, -> \v {
    lowered-away(v, '$x')
}, 'a non-escaping bound declaration is lowered to a local';

qast-is 'my @a = 1,2; say @a.sum', :full, -> \v {
    lowered-away(v, '@a')
}, 'a non-escaping array is lowered to a local';

qast-is 'my %h = a => 1; say %h.elems', :full, -> \v {
    lowered-away(v, '%h')
}, 'a non-escaping hash is lowered to a local';

qast-is 'sub f($a) { $a + 1 }; say f(1)', :full, -> \v {
    lowered-away(v, '$a')
}, 'a non-escaping parameter is lowered to a local';

qast-is 'my $x = 1; my $c = { $x }; say $c()', :full, -> \v {
    kept-lexical(v, '$x') and not lowered-away(v, '$x')
}, 'a block-captured scalar keeps its lexical';

qast-is 'use MONKEY-SEE-NO-EVAL; my $x = 1; say EVAL "1"', :full, -> \v {
    kept-lexical(v, '$x')
}, 'EVAL in scope keeps every lexical addressable by name';

qast-is 'use MONKEY-SEE-NO-EVAL; my $x = 1; say "1".EVAL', :full, -> \v {
    kept-lexical(v, '$x')
}, 'the EVAL method in scope keeps every lexical addressable by name';

qast-is 'my $x is dynamic = 1; say $x', :full, -> \v {
    kept-lexical(v, '$x')
}, 'an is-dynamic declaration keeps its lexical';

qast-is 'my $*d = 1; say $*d', :full, -> \v {
    kept-lexical(v, '$*d')
}, 'a dynamic-twigil declaration keeps its lexical';

qast-is 'my &g = { 42 }; say g()', :full, -> \v {
    kept-lexical(v, '&g')
}, 'a callable declaration keeps its lexical for by-name callee lookup';

qast-is 'my int $i = 5; say $i', :full, -> \v {
    qast-var-decl(v, '$i', 'var')
}, 'a native scalar keeps a declaration under its own name';

qast-is 'my $x = 1; say MY::<$x>', :full, -> \v {
    kept-lexical(v, '$x')
}, 'a pseudo-package access in scope keeps every lexical';

qast-is 'multi f(Int $i) { my $t = 1; callsame; $t }; multi f(Any $a) { 2 }; say f(1)', :full, -> \v {
    kept-lexical(v, '$t')
}, 'callsame in scope keeps the enclosing lexicals';

qast-is 'my $x = "a"; say "a" ~~ /a/; say $x', :full, -> \v {
    kept-lexical(v, '$x')
}, 'a regex in scope keeps the enclosing lexicals';

qast-is 'my $x will leave { 1 } = 5; say $x', :full, -> \v {
    kept-lexical(v, '$x')
}, 'a will phaser keeps its variable addressable by name';

qast-is 'my ($a, $b) := (3, 4); say $a', :full, -> \v {
    kept-lexical(v, '$a')
}, 'a signature binding keeps its targets addressable for the binder';

qast-is 'my $x = 2..4; my @r; $x ==> map(* + 1) ==> @r; say @r', :full, -> \v {
    kept-lexical(v, '$x')
}, 'a feed keeps the lexicals its stages reference';

# Behavior stays identical under lowering.

is (1..3).map(-> $i { sub { $i * 10 } }).map({ .() }).join(','), '10,20,30',
    'closures capture distinct per-iteration variables';

{
    my ($a, $b) = (3, 4);
    is "$a $b", '3 4', 'list assignment reaches the declared targets';
}

{
    my ($a, $b) := (3, 4);
    is "$a $b", '3 4', 'list binding reaches the declared targets';
}

{
    sub lex2local-callee() { CALLER::<$x> }
    sub lex2local-caller() { my $x = 1; lex2local-callee() }
    throws-like { lex2local-caller() }, X::Caller::NotDynamic,
        'CALLER:: access to a lowered lexical dies the same way';
}

{
    sub with-copy($a is copy) { $a = 5; $a }
    is with-copy(1), 5, 'an is-copy parameter assigns inside its frame';
}

{
    sub counting() { state $n = 0; ++$n }
    counting();
    is counting(), 2, 'a state variable keeps its per-code storage';
}

# The transform obeys the optimizer switch.

my $stage = nqp::getcomp('Raku').qast-stage;
is-run 'my $x = 1; say $x',
    'no declaration is lowered under --optimize=off',
    :compiler-args['--optimize=off', "--target=$stage"],
    :out({ $_ !~~ / '__lowered' / });

is-run 'my $x = 1; say $x',
    'the QAST of an optimized compilation shows the lowered local',
    :compiler-args["--target=$stage"],
    :out({ $_ ~~ / '__lowered' / });
