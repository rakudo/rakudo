use MONKEY-SEE-NO-EVAL;
use Test;
use nqp;

plan 8;

# A lexical is emitted by name, so a nested use of an outer name binds to the
# declaration its enclosing scope makes of that name further on.

is (try EVAL q:to/CODE/), "inner", 'a closure using a name its routine declares later reads that declaration';
    my $a = "outer";
    sub f() { my &g = { $a }; my $a = "inner"; g() }
    f()
    CODE

is (try EVAL q:to/CODE/), 3, 'a closure using an array name its routine declares later reads that declaration';
    my @b = 1;
    sub h() { my &g = { @b.elems }; my @b = 1, 2, 3; g() }
    h()
    CODE

is (try EVAL q:to/CODE/), "inner", 'a block using a name its enclosing block declares later reads that declaration';
    my $c = "outer";
    sub k() { my $got; for ^1 { my &g = { $c }; my $c = "inner"; $got = g() }; $got }
    k()
    CODE

is (try EVAL q:to/CODE/), 3, 'a closure using a hash name its routine declares later reads that declaration';
    my %d = a => 1;
    sub hh() { my &g = { %d.elems }; my %d = a => 1, b => 2, c => 3; g() }
    hh()
    CODE

is (try EVAL q:to/CODE/), "inner", 'a closure two blocks deep using a name its routine declares later reads that declaration';
    my $f = "outer";
    sub deep() { my &g = { my &h = { $f }; h() }; my $f = "inner"; g() }
    deep()
    CODE

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    is (try EVAL q:to/CODE/), "term", 'a closure using a constant name its routine declares later as a term reads the term';
        constant t = "const";
        sub tf() { my &g = { t }; my \t = "term"; g() }
        tf()
        CODE

    is (try EVAL q:to/CODE/), "term", 'a closure using an enum value name its routine declares later as a term reads the term';
        enum E <ev>;
        sub ef() { my &g = { ev }; my \ev = "term"; g() }
        ef()
        CODE

    is (try EVAL q:to/CODE/), "inner", 'a default closure using a name a later parameter declares reads the parameter';
        my $e = "outer";
        sub pf(&g = { $e }, $e = "inner") { g() }
        pf()
        CODE
}
else {
    skip 'the legacy frontend does not read the later declaration in these', 3;
}
