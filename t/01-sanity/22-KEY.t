use v6;

use Test;

plan 126;

my %h            = a => 42, b => 666;
my Int %hi       = a => 42, b => 666;
my Int %hia{Any} = a => 42, b => 666;

for $%h, Any, $%hi, Int, $%hia, Int -> \h, \T {
    my $name = h.^name;

    is h.AT-KEY("a"),        42, "$name.AT-KEY";
    is (h.AT-KEY("b") = 65), 65, "$name.AT-KEY =";
    is h.AT-KEY("b"),        65, "$name.AT-KEY (changed)";

    ok h.EXISTS-KEY("a"),  "$name.EXISTS-KEY (existing)";
    ok !h.EXISTS-KEY("c"), "!$name.EXISTS-KEY (non-existing)";

    is h.ASSIGN-KEY("a",33), 33, "$name.ASSIGN-KEY (existing)";
    is h.AT-KEY("a"),        33, "$name.AT-KEY (existing ASSIGN-KEY)";
    is h.ASSIGN-KEY("c",65), 65, "$name.ASSIGN-KEY (non-existing)";
    is h.AT-KEY("c"),        65, "$name.AT-KEY (non-existing ASSIGN-KEY)";

    my $a = 45;
    my $d = 67;
    is h.BIND-KEY("a",$a), 45, "$name.BIND-KEY (existing)";
    is h.AT-KEY("a"),      45, "$name.AT-KEY (existing BIND-KEY)";
    $a = 90;
    is h.AT-KEY("a"),      90, "$name.AT-KEY (changed existing BIND-KEY)";

    is h.BIND-KEY("d",$d), 67, "$name.BIND-KEY (non-existing)";
    is h.AT-KEY("d"),      67, "$name.AT-KEY (non-existing BIND-KEY)";
    $d = 56;
    is h.AT-KEY("d"),      56, "$name.AT-KEY (changed non-existing BIND-KEY)";

    is h.DELETE-KEY("a"),  90, "$name.DELETE-KEY (existing)";
    ok !h.EXISTS-KEY("a"),     "!$name.EXISTS-KEY (existing DELETE-KEY)";
    is h.DELETE-KEY("e"),   T, "$name.DELETE-KEY (non-existing)";
    ok !h.EXISTS-KEY("e"),     "!$name.EXISTS-KEY (non-existing DELETE-KEY)";
}

{
    my $a;
    ok !$a.EXISTS-KEY("a"),       "\$a.EXISTS-KEY (undefined)";
    is $a.AT-KEY("a"),       Any, "\$a.AT-KEY (undefined)";
    is ($a.AT-KEY("a") = 42), 42, "\$a.AT-KEY = (undefined)";
    is $a.AT-KEY("a"),        42, "\$a.AT-KEY (defined)";
    ok $a.EXISTS-KEY("a"),        "\$a.EXISTS-KEY (defined)";
    is $a.DELETE-KEY("a"),    42, "\$a.DELETE-KEY (defined)";
    ok !$a.EXISTS-KEY("a"),       "\$a.EXISTS-KEY (after delete)";
}

{
    my $a;
    is ($a.ASSIGN-KEY("a",42)), 42, "\$a.ASSIGN-KEY (undefined)";
    is $a.AT-KEY("a"),          42, "\$a.AT-KEY (defined)";
    is $a.DELETE-KEY("a"),      42, "\$a.DELETE-KEY (defined)";
    ok !$a.EXISTS-KEY("a"),         "\$a.EXISTS-KEY (after delete)";
}

{
    my $a;
    my $b = 42;
    is ($a.BIND-KEY("a",$b)), 42, "\$a.BIND-KEY (undefined)";
    is $a.AT-KEY("a"),        42, "\$a.AT-KEY (defined)";
    $b = 65;
# todo, no fudging in sanity
#    is $a.AT-KEY("a"),        65, "\$a.AT-KEY (defined)";
#    is $a.DELETE-KEY("a"),    65, "\$a.DELETE-KEY (defined)";
#    ok !$a.EXISTS-KEY("a"),       "\$a.EXISTS-KEY (after delete)";
}

{
    my $a;
    is $a.DELETE-KEY("a"), Nil, "\$a.DELETE-KEY (undefined)";  # not sure ok
    ok !$a.EXISTS-KEY("a"),     "\$a.EXISTS-KEY (after delete)";
}

{
    my $a = 42;
    my $s = <a b c>.Set;
    ok $s.EXISTS-KEY("a"),   "\$s.EXISTS-KEY";
    ok !$s.EXISTS-KEY("d"),  "!\$s.EXISTS-KEY";
    is $s.AT-KEY("a"), True, "\$s.AT-KEY";
    throws_like { $s.BIND-KEY("a",$a) },X::Bind,:target<Set>,"\$s.BIND-KEY";
    is $s.AT-KEY("a"), True, "\$s.AT-KEY (after bind)";
    throws_like { $s.DELETE-KEY("a") },
      X::Immutable, :method<DELETE-KEY>, :typename<Set>, "\$s.DELETE-KEY";
    is $s.AT-KEY("a"), True, "\$s.AT-KEY (after delete)";
    throws_like { $s.ASSIGN-KEY("a",False) },
      X::Assignment::RO, :typename<Set>, "\$s.ASSIGN-KEY";
    is $s.AT-KEY("a"), True, "\$s.AT-KEY (after assignment)";
}

{
    my $a = True;
    my $sh = <a b c>.SetHash;
    ok $sh.EXISTS-KEY("a"),   "\$sh.EXISTS-KEY";
    ok !$sh.EXISTS-KEY("d"),  "!\$sh.EXISTS-KEY";
    is $sh.AT-KEY("a"), True, "\$sh.AT-KEY";
    throws_like { $sh.BIND-KEY("a",$a) },
      X::Bind,:target<SetHash>,"\$sh.BIND-KEY";
    is $sh.AT-KEY("a"),          True, "\$sh.AT-KEY (after bind)";
    is $sh.DELETE-KEY("a"),      True, "\$sh.DELETE-KEY";
    is $sh.EXISTS-KEY("a"),     False, "\$sh.EXISTS-KEY (after delete)";
    is $sh.ASSIGN-KEY("a",True), True, "\$sh.ASSIGN-KEY";
    is $sh.AT-KEY("a"),          True, "\$sh.AT-KEY (after assignment)";
}

{
    my $a = 42;
    my $b = <a b b c c c>.Bag;
    ok $b.EXISTS-KEY("a"),  "\$b.EXISTS-KEY";
    ok !$b.EXISTS-KEY("d"), "!\$b.EXISTS-KEY";
    is $b.AT-KEY("a"), 1,   "\$b.AT-KEY";
    throws_like { $b.BIND-KEY("a",$a) },X::Bind,:target<Bag>,"\$b.BIND-KEY";
    is $b.AT-KEY("a"), 1, "\$b.AT-KEY (after bind)";
    throws_like { $b.DELETE-KEY("a") },
      X::Immutable, :method<DELETE-KEY>, :typename<Bag>, "\$b.DELETE-KEY";
    is $b.AT-KEY("a"), 1, "\$b.AT-KEY (after delete)";
    throws_like { $b.ASSIGN-KEY("a",42) },
      X::Assignment::RO, :typename<Bag>, "\$b.ASSIGN-KEY";
    is $b.AT-KEY("a"), 1, "\$b.AT-KEY (after assignment)";
}

{
    my $a = 42;
    my $bh = <a b b c c c>.BagHash;
    ok $bh.EXISTS-KEY("a"),   "\$bh.EXISTS-KEY";
    ok !$bh.EXISTS-KEY("d"),  "!\$bh.EXISTS-KEY";
    is $bh.AT-KEY("a"), 1, "\$bh.AT-KEY";
    throws_like { $bh.BIND-KEY("a",$a) },
      X::Bind,:target<BagHash>,"\$bh.BIND-KEY";
    is $bh.AT-KEY("a"),          1, "\$bh.AT-KEY (after bind)";
    is $bh.DELETE-KEY("a"),      1, "\$bh.DELETE-KEY";
    is $bh.EXISTS-KEY("a"),  False, "\$bh.EXISTS-KEY (after delete)";
    is $bh.ASSIGN-KEY("a",$a),  $a, "\$bh.ASSIGN-KEY";
    is $bh.AT-KEY("a"),         $a, "\$bh.EXISTS-KEY (after assignment)";
}

{
    my $a = 42;
    my $m = <a b b c c c>.Mix;
    ok $m.EXISTS-KEY("a"),  "\$m.EXISTS-KEY";
    ok !$m.EXISTS-KEY("d"), "!\$m.EXISTS-KEY";
    is $m.AT-KEY("a"), 1,   "\$m.AT-KEY";
    throws_like { $m.BIND-KEY("a",$a) },X::Bind,:target<Mix>,"\$m.BIND-KEY";
    is $m.AT-KEY("a"), 1, "\$m.AT-KEY (after bind)";
    throws_like { $m.DELETE-KEY("a") },
      X::Immutable, :method<DELETE-KEY>, :typename<Mix>, "\$m.DELETE-KEY";
    is $m.AT-KEY("a"), 1, "\$m.AT-KEY (after delete)";
    throws_like { $m.ASSIGN-KEY("a",42) },
      X::Assignment::RO, :typename<Mix>, "\$m.ASSIGN-KEY";
    is $m.AT-KEY("a"), 1, "\$m.AT-KEY (after assignment)";
}

{
    my $a = 42.5;
    my $mh = <a b b c c c>.MixHash;
    ok $mh.EXISTS-KEY("a"),  "\$mh.EXISTS-KEY";
    ok !$mh.EXISTS-KEY("d"), "!\$mh.EXISTS-KEY";
    is $mh.AT-KEY("a"), 1,   "\$mh.AT-KEY";
    throws_like { $mh.BIND-KEY("a",$a) },
      X::Bind,:target<MixHash>,"\$mh.BIND-KEY";
    is $mh.AT-KEY("a"),          1, "\$mh.AT-KEY (after bind)";
    is $mh.DELETE-KEY("a"),      1, "\$mh.DELETE-KEY";
    is $mh.EXISTS-KEY("a"),  False, "\$mh.EXISTS-KEY (after delete)";
    is $mh.ASSIGN-KEY("a",$a),  $a, "\$mh.ASSIGN-KEY";
    is $mh.AT-KEY("a"),         $a, "\$mh.EXISTS-KEY (after assignment)";
}
