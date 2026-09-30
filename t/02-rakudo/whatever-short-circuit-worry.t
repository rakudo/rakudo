use Test;
use nqp;
use MONKEY-SEE-NO-EVAL;

plan 36;

todo 'the legacy frontend does not worry about an unprimed first operand', 22
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like ｢use fatal; my &f = * > 0 && * < 9｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<&&>,
    'a WhateverCode left of && draws a worry';
throws-like ｢use fatal; my &f = (* > 0 and * < 9)｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<and>,
    'a WhateverCode left of and draws a worry';
throws-like ｢use fatal; my &f = *.succ || 0｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<||>,
    'a WhateverCode left of || draws a worry';
throws-like ｢use fatal; my &f = (*.succ or 0)｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<or>,
    'a WhateverCode left of or draws a worry';
throws-like ｢use fatal; my &f = *.succ // 0｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<//>,
    'a WhateverCode left of // draws a worry';
throws-like ｢use fatal; my &f = *.succ ^^ 0｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<^^>,
    'a WhateverCode left of ^^ draws a worry';
throws-like ｢use fatal; my &f = (*.succ xor 0)｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<xor>,
    'a WhateverCode left of xor draws a worry';
throws-like ｢use fatal; my $x = (*.succ andthen 1)｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<andthen>,
    'a WhateverCode left of andthen draws a worry';
throws-like ｢use fatal; my $x = (*.succ orelse 1)｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<orelse>,
    'a WhateverCode left of orelse draws a worry';
throws-like ｢use fatal; my $x = (*.succ notandthen 1)｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<notandthen>,
    'a WhateverCode left of notandthen draws a worry';
throws-like ｢use fatal; my $x = * // 0｣, X::Whatever::ShortCircuit,
    :what<Whatever>, :operator<//>,
    'a Whatever left of // draws a worry';
throws-like ｢use fatal; my &f = (*.succ) || 0｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<||>,
    'a parenthesized WhateverCode left of || draws a worry';
throws-like ｢use fatal; my &f = *.succ [||] 0｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<||>,
    'a WhateverCode left of [||] draws a worry';
throws-like ｢use fatal; my &f = *.succ S|| 0｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<||>,
    'a WhateverCode left of S|| draws a worry';
throws-like ｢use fatal; my &f = 0 R|| *.succ｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<||>,
    'a WhateverCode right of R|| draws a worry, as || tests it first';
throws-like ｢use fatal; my $x = (0 Randthen *.succ)｣, X::Whatever::ShortCircuit,
    :what<WhateverCode>, :operator<andthen>,
    'a WhateverCode right of Randthen draws a worry, as andthen tests it first';
throws-like ｢use fatal; my &f = * > 0 && * < 9｣, X::Whatever::ShortCircuit,
    message => { .contains('WhateverCode') && .contains('&&') },
    'the worry names the WhateverCode and the operator';
throws-like "use fatal;\nmy \&f = (*.succ\n    xor 0\n    xor 0)", X::Whatever::ShortCircuit,
    :line(2), 'the worry is reported at the line of the operand';
throws-like "use fatal;\nmy \&f = *.substr(\n    1).chars && 1;", X::Whatever::ShortCircuit,
    :line(2), 'the worry is reported at the line its operand starts on';
throws-like ｢use fatal; sub f($p where * > 0 && * < 9) { }｣, X::Whatever::ShortCircuit,
    :operator<&&>, 'a WhateverCode left of && in a parameter where draws one worry';
throws-like ｢use fatal; my $x where (* > 0 and * < 9) = 5｣, X::Whatever::ShortCircuit,
    :operator<and>, 'a WhateverCode left of and in a variable where draws one worry';
throws-like ｢use fatal; sub infix:<&&>($, $) { 1 }; my &f = * > 0 && * < 9｣,
    X::Whatever::ShortCircuit, :operator<&&>,
    'a WhateverCode left of && draws a worry beside a && declared in scope, which && does not call';

eval-lives-ok ｢use fatal; my &f = 0 || *.succ｣,
    'a WhateverCode right of || draws no worry';
eval-lives-ok ｢use fatal; my &f = 0 ^^ *.succ｣,
    'a WhateverCode right of ^^ draws no worry';
eval-lives-ok ｢use fatal; my $x = (42 andthen *.succ)｣,
    'a WhateverCode right of andthen draws no worry';
eval-lives-ok ｢use fatal; my &f = *.succ R|| 0｣,
    'a WhateverCode left of R|| draws no worry, as || tests it last';
eval-lives-ok ｢use fatal; my &f = *.succ !&& 1｣,
    'a WhateverCode left of !&& draws no worry, as !&& primes it';
eval-lives-ok ｢use fatal; my &f = *.succ Z|| 0｣,
    'a WhateverCode left of Z|| draws no worry, as Z|| primes it';
eval-lives-ok ｢use fatal; sub infix:<||>($a, $b) { $b }; my $x = 0 R|| *.succ｣,
    'a WhateverCode right of R|| draws no worry when R|| calls a || declared in scope';

# A double closure sorry covers a WhateverCode that a short-circuit operator
# in the block tests first.
{
    try EVAL ｢sub f($a where {* < 5 and * > 9}) { }｣;
    isa-ok $!, X::Syntax::Malformed, 'a parameter where block that is a double closure draws only the sorry';
}
{
    try EVAL ｢my subset S of Int where {* < 5 and * > 9}｣;
    isa-ok $!, X::Syntax::Malformed, 'a subset where block that is a double closure draws only the sorry';
}
{
    try EVAL ｢say {* < 5 and * > 9}()｣;
    isa-ok $!, X::Syntax::Malformed, 'a block called in place that is a double closure draws only the sorry';
}
todo 'the legacy frontend misses a double closure under fatal', 4
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
{
    try EVAL ｢use fatal; my subset S of Int where {* < 5 and * > 9}｣;
    isa-ok $!, X::Syntax::Malformed, 'a subset where block that is a double closure draws only the sorry under fatal';
}
{
    try EVAL ｢use fatal; my $x where {* < 5 and * > 9} = 1｣;
    isa-ok $!, X::Syntax::Malformed, 'a variable where block that is a double closure draws only the sorry under fatal';
}
{
    try EVAL ｢use fatal; class A { has $.x where {* < 5 and * > 9} }｣;
    isa-ok $!, X::Syntax::Malformed, 'an attribute where block that is a double closure draws only the sorry under fatal';
}
{
    try EVAL ｢use fatal; say {* < 5 and * > 9}()｣;
    isa-ok $!, X::Syntax::Malformed, 'a block called in place that is a double closure draws only the sorry under fatal';
}
