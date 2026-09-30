use Test;
use nqp;
use MONKEY-SEE-NO-EVAL;

plan 130;

# A short-circuit operator takes a Whatever operand as a value.

is (0 // *), 0, 'a defined left side of // is returned, not curried';
is (Any // *), *, 'an undefined left side of // yields the literal Whatever';
is (0 orelse *), 0, 'orelse returns its defined left side';
is (0 and *), 0, 'and returns its false left side';
is (5 or *), 5, 'or returns its true left side';
isa-ok (0 // *), Int, 'a short-circuit result is a plain value, not a WhateverCode';
isa-ok (0 ^^ *), Whatever, '^^ returns its only true operand, a Whatever, as is';

# The reduction that motivated this: an index of 0 through // must stay 0.
{
    my @a = 10, 20, 30;
    my $idx = @a.first(:k, * >= 10);
    is @a.skip($idx // *).elems, 3, 'skip(0 // *) keeps every element';
}

is (42 andthen *.succ), 43,
    'andthen calls a WhateverCode right side with its defined left side';
is (Any orelse *.raku), 'Any',
    'orelse calls a WhateverCode right side with its undefined left side';
is (Any notandthen *.raku), 'Any',
    'notandthen calls a WhateverCode right side with its undefined left side';
is (42 andthen *.succ andthen *.succ), 44,
    'chained andthen calls each WhateverCode with the value before it';
is (42 andthen *.succ + 1), 44,
    'andthen calls a WhateverCode built from a method call and an operator';
given 100 {
    is (42 andthen * + $_), 142,
        'andthen calls a WhateverCode right side without setting the topic it closes over';
}
todo 'the legacy frontend returns a parenthesized WhateverCode uncalled', 2
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is (42 andthen (*.succ)), 43,
    'andthen calls a parenthesized WhateverCode right side with its left side';
is (42 andthen ((*.succ))), 43,
    'andthen calls a WhateverCode right side in doubled parentheses with its left side';
todo 'the legacy frontend primes a WhateverCode operand of a bracketed andthen', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is (42 [andthen] *.succ), 43,
    '[andthen] calls a WhateverCode right side with its left side';
is-deeply (Any andthen *.succ), Empty,
    'andthen skips a WhateverCode right side after an undefined left side';
is (42 orelse *.succ), 42,
    'orelse returns its defined left side over a WhateverCode right side';
is-deeply (42 notandthen *.succ), Empty,
    'notandthen skips a WhateverCode right side after a defined left side';
is (0 // *.succ), 0,
    '// returns its defined left side over a WhateverCode right side';
{
    my $n = 0;
    my &f = (++$n and *.succ);
    f(1);
    is f(41), 42, 'and returns a WhateverCode right side as is';
    is $n, 1, 'and runs its left side once rather than priming over it';
}
is (1 and *.succ)(41), 42, 'and returns a WhateverCode right side of a true literal as is';
{
    my $n = 0;
    my &f = ($n++ or *.succ);
    f(1);
    is f(41), 42, 'or returns a WhateverCode right side as is';
    is $n, 1, 'or runs its left side once rather than priming over it';
}
is-deeply (1 xor *.succ), Nil,
    'xor of a true left side and a WhateverCode right side is Nil';
is-deeply (1 ^^ *.succ), Nil,
    '^^ of a true left side and a WhateverCode right side is Nil';
{
    no worries;
    is (* > 1 and * < 9)(20), False,
        'and between two WhateverCodes returns the right one rather than priming both';
}

# The meta-op forms of andthen, orelse, and notandthen call a WhateverCode
# operand with the other operand as the plain operator does.
is ([andthen] 42, *.succ), 43,
    '[andthen] calls a WhateverCode with the value before it';
is-deeply ([\andthen] 42, *.succ).List, (42, 43),
    '[\andthen] calls a WhateverCode with the value before it';
is ([orelse] Any, *.raku), 'Any',
    '[orelse] calls a WhateverCode with the undefined value before it';
is-deeply ((1, 2) Zandthen (*.succ, *.pred)).List, (2, 1),
    'Zandthen calls each WhateverCode element with its left element';
is-deeply ((1, 2) Xandthen (*.succ, *.pred)).List, (2, 0, 3, 1),
    'Xandthen calls each WhateverCode element with each left element';
todo 'the legacy frontend primes a WhateverCode operand of a zip, hyper, or reverse andthen', 5
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is-deeply (42 Zandthen *.succ).List, (43,),
    'Zandthen calls a WhateverCode operand with the left element';
is (42 »andthen« *.succ), 43,
    '»andthen« calls a WhateverCode operand with the left side';
is (*.succ Randthen 42), 43,
    'Randthen calls a WhateverCode left side with its defined right side';
is-deeply (*.succ ZRandthen 42).List, (43,),
    'ZRandthen calls a WhateverCode left operand with the right element';
is (*.succ »Randthen« 42), 43,
    '»Randthen« calls a WhateverCode left operand with the right side';

# The reverse and assign forms take a Whatever or WhateverCode operand as a
# value, like the plain operator.
is (*.succ R// 1), 1,
    'R// returns its defined right side over a WhateverCode left side';
is-deeply (*.succ R^^ 1), Nil,
    'R^^ of a WhateverCode left side and a true right side is Nil';
is ((*.succ) R^^ 0)(41), 42,
    'R^^ returns a parenthesized WhateverCode left side as is';
todo 'the legacy frontend primes a Whatever operand of R^^ and leaks a thunk from [\R^^]', 2
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
isa-ok (* R^^ 0), Whatever, 'R^^ returns a Whatever left side as ^^ does';
isa-ok ([\R^^] *.succ, 0)[0], WhateverCode, '[\R^^] yields a WhateverCode first operand as is';
todo 'the legacy frontend primes a WhateverCode operand of these forms', 4
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
{
    my $x = 0;
    $x ^^= (*.succ);
    isa-ok $x, WhateverCode, '^^= assigns a parenthesized WhateverCode right side to a false left side';
}
{
    my $x = 0;
    $x ^^= *.succ;
    isa-ok $x, WhateverCode, '^^= assigns a WhateverCode right side to a false left side';
}
{
    my $x = 0;
    $x [^^]= *.succ;
    isa-ok $x, WhateverCode, '[^^]= assigns a WhateverCode right side to a false left side';
}
{
    my $x = 0;
    $x xor= *.succ;
    isa-ok $x, WhateverCode, 'xor= assigns a WhateverCode right side to a false left side';
}

# An assignment meta-op assigns a WhateverCode right side as a value.
{
    my $x;
    $x //= *.succ;
    is $x(41), 42, '//= assigns a WhateverCode right side to an undefined left side';
}
{
    my $x = 1;
    $x and= *.succ;
    is $x(41), 42, 'and= assigns a WhateverCode right side to a true left side';
}
{
    my $x = 0;
    $x or= *.succ;
    is $x(41), 42, 'or= assigns a WhateverCode right side to a false left side';
}
{
    my $x;
    $x orelse= *.succ;
    is $x(41), 42, 'orelse= assigns a WhateverCode right side to an undefined left side';
}
{
    my $x = 42;
    $x andthen= *.succ;
    is $x(41), 42, 'andthen= assigns a WhateverCode right side to a defined left side';
}
{
    my $x;
    $x notandthen= *.succ;
    is $x(41), 42, 'notandthen= assigns a WhateverCode right side to an undefined left side';
}
{
    my $x = 42;
    $x [andthen]= *.succ;
    is $x(41), 42, '[andthen]= assigns a WhateverCode right side to a defined left side';
}
{
    $_ = 10;
    my $x = 1;
    $x andthen= * + $_;
    $_ := 100;
    is $x(5), 105, 'andthen= assigns a WhateverCode that sees the topic it closes over';
}
{
    my %h;
    %h<k> //= *.succ;
    is %h<k>(41), 42, '//= assigns a WhateverCode right side to an undefined hash element';
}
todo 'the legacy frontend primes a Whatever operand of ^^= and xor=, and cannot compile a hyper or zip assign', 5
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
{
    my $x = 0;
    $x ^^= *;
    isa-ok $x, Whatever, '^^= assigns a Whatever right side to a false left side';
}
{
    my $x = 0;
    $x xor= *;
    isa-ok $x, Whatever, 'xor= assigns a Whatever right side to a false left side';
}
isa-ok (try EVAL ｢my @a = Any, 1; @a »//=» *; @a[0]｣), Whatever,
    '»//=» assigns a Whatever right side to an undefined element';
isa-ok (try EVAL ｢my @a = Any, 1; @a »//=» *.succ; @a[0]｣), WhateverCode,
    '»//=» assigns a WhateverCode right side to an undefined element';
is (try EVAL ｢my @a = 1; @a Z&&= *.succ; @a[0](41)｣), 42,
    'Z&&= assigns a WhateverCode right side to a true element';

# A negated short-circuit yields a Bool, so it primes a Whatever or
# WhateverCode operand.
is (0 !^^ *.succ)(41), False, '!^^ primes a WhateverCode operand';
is (0 !^^ (*.succ))(41), False, '!^^ primes a parenthesized WhateverCode operand';
is (*.succ !and 1)(0), False, '!and primes a WhateverCode operand';
todo 'the legacy frontend takes a WhateverCode right of a negated && as a value', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
isa-ok (1 !&& *.succ), WhateverCode, '!&& primes a WhateverCode right side';
is-deeply (1, 0, 2, "").grep(* !^^ True).List, (1, 2), '!^^ primes a Whatever operand';
is-deeply (1, 0, 2, "").grep(* !&& True).List, (0, ""), '!&& primes a Whatever operand';

# Under a zip, cross, or hyper, a short-circuit operator primes like any other
# operator, except for a WhateverCode that andthen, orelse, or notandthen calls
# with the other operand.
is-deeply (1 Z&& *.succ)(41).List, (42,), 'Z&& primes a WhateverCode operand';
is-deeply (*.succ Zandthen 42)(41).List, (42,), 'Zandthen primes a WhateverCode on its left';
is-deeply (5 ZRandthen *.succ)(41).List, (5,), 'ZRandthen primes a WhateverCode on its right, which andthen tests';
is (5 »Randthen« *.succ)(41), 5, '»Randthen« primes a WhateverCode on its right, which andthen tests';
throws-like ｢sub ($a where {* < 5 Zandthen 1}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode left of a Zandthen in a where block is a double closure';
is-deeply (1 X&& *.succ)(41).List, (42,), 'X&& primes a WhateverCode operand';
is (1 »&&« *.succ)(41), 42, '»&&« primes a WhateverCode operand';
is-deeply (Any Z// *.succ)(41).List, (42,), 'Z// primes a WhateverCode operand';
is-deeply (Any X// *.succ)(41).List, (42,), 'X// primes a WhateverCode operand';
is-deeply (0 Z^^ *.succ)(41).List, (42,), 'Z^^ primes a WhateverCode operand';
is-deeply (0 X^^ *)(1).List, (1,), 'X^^ primes a Whatever operand';
is-deeply (0 Z|| *)(5).List, (5,), 'Z|| primes a Whatever operand';
is-deeply (Any Zorelse *)(5).List, (5,), 'Zorelse primes a Whatever operand';

# Non-short-circuiting operators still curry as before.
{
    my $wc = 5 + *;
    isa-ok $wc, WhateverCode, 'a Whatever under + still curries';
    is $wc(3), 8, 'the curried WhateverCode applies';
}
{
    my @a = 1, 2, 3;
    is @a[* - 1], 3, 'a Whatever in a subscript still curries';
}
is (* ~~ 3).WHAT, WhateverCode, 'a Whatever under ~~ still curries';

# A where block's value is only tested, so a WhateverCode that decides it or
# that it can return makes it a double closure.
throws-like ｢sub ($a where {* < 5 and * > 9}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading an and in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 or * > 9}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading an or in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 && * > 9}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading a && in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 || * > 9}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading a || in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 ?? 1 !! 0}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode as the condition of a ternary in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 && 1 || 0}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading nested short-circuits in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 ?? 1 !! 0 and 1}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading a ternary under an and in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 [&&] * > 9}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading a [&&] in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 !and 1}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode under a negated and in a where block is a double closure';
throws-like ｢sub ($a where {1 Rand * > 9}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode that a reversed and tests first in a where block is a double closure';
throws-like ｢sub ($a where {1 Randthen 2 Randthen * < 5}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode that a reversed andthen of three operands tests first in a where block is a double closure';
todo 'the legacy frontend does not look through a sequenced reversed and', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like ｢sub ($a where {* > 9 SRand 1}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode that a sequenced reversed and can return in a where block is a double closure';
throws-like ｢say {* > 9 R[Rand] 1}()｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode that a doubly reversed and tests first in a block called in place is a double closure';
todo 'the legacy frontend only looks at a leading and, or, &&, ||, or ternary condition', 20
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like ｢sub ($a where { ($_ < 0 or * > 10).Bool }) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode under a .Bool call in a where block is a double closure';
throws-like ｢sub ($a where { not ($_ < 0 or * > 10) }) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode under not in a where block is a double closure';
throws-like ｢sub ($a where { ?^($_ < 0 or * > 10) }) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode under a prefix ?^ in a where block is a double closure';
throws-like ｢sub ($a where { ($_ < 0 or * > 10) ?| False }) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode left of a ?| in a where block is a double closure';
throws-like ｢sub ($a where {$_ > 5 xor * < 0}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode right of an xor in a where block is a double closure';
throws-like ｢sub ($a where { so ($_ < 0 or * > 10) }) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode under so in a where block is a double closure';
throws-like ｢sub ($a where { ?($_ // * > 10) }) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode under ? in a where block is a double closure';
throws-like ｢sub ($a where { !($_ < 0 or * > 10) }) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode under ! in a where block is a double closure';
throws-like ｢sub ($a where { ($_ < 0 or * > 10).so }) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode under a .so call in a where block is a double closure';
throws-like ｢sub ($a where { $_ andthen ($_ > 0 and * > 5) }) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode that a topic block right of andthen returns in a where block is a double closure';
throws-like ｢sub ($a where { $_ andthen (1 && * > 1) }) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode right of a && right of andthen in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 S&& 1}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading a sequenced && in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 // 1}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading a // in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 orelse 1}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading an orelse in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 andthen 1}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading an andthen in a where block is a double closure';
throws-like ｢sub ($a where {* < 5 xor * > 9}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode under an xor in a where block is a double closure';
throws-like ｢sub ($a where {1 and * > 9}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode right of an and in a where block is a double closure';
throws-like ｢sub ($a where {$_ < 5 and * > 9}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode after a topic test in a where block is a double closure';
throws-like ｢sub ($a where {$_ > 5 or * < 0}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode right of an or in a where block is a double closure';
throws-like ｢sub ($a where {$_ ?? * > 5 !! False}) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode in a ternary branch of a where block is a double closure';
eval-lives-ok ｢sub ($a where {$_ < 5 and $_ > 1}) { }｣,
    'a where block without a WhateverCode is not reported as a double closure';
eval-lives-ok ｢sub ($a where {$_ < 5 andthen * > 1}) { }｣,
    'a WhateverCode that andthen calls in a where block is not reported as a double closure';
eval-lives-ok ｢sub (@a where { .first(* > 5).so }) { }｣,
    'a boolifier over a call that takes a WhateverCode is not reported as a double closure';

# A block called in place is a double closure when a WhateverCode is its
# value or decides it.
throws-like ｢say {* < 5 and * > 9}()｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading an and in a called block is a double closure';
throws-like ｢say {* < 5 and * > 9}.()｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading an and in a block called with .() is a double closure';
throws-like ｢say ({* < 5 and * > 9})()｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading an and in a parenthesized called block is a double closure';
throws-like ｢say {* < 5 && * > 9}()｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode leading a && in a called block is a double closure';
throws-like ｢say {* < 5 ?? 1 !! 0}()｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode as the condition of a ternary in a called block is a double closure';
throws-like ｢say (({* < 5}))()｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a block holding only a WhateverCode called in doubled parentheses is a double closure';
eval-lives-ok ｢my &f = {1 && * + 1}(); die unless f(1) == 2｣,
    'a called block may return a WhateverCode it builds';
todo 'the legacy frontend does not look inside an andthen chain', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like ｢say { $_ andthen (1 && * > 5) andthen 2 }(3)｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode that the middle of an andthen chain tests in a called block is a double closure';
is { $_ andthen *.succ andthen $_ * 2 }(3), 8,
    'a WhateverCode that the middle of an andthen chain calls in a called block is no double closure';
