use Test;
use nqp;
use MONKEY-SEE-NO-EVAL;

plan 14;

throws-like ｢my subset S of Int where {* < 5}｣,
    X::Syntax::Malformed, :what{.contains: 'closure'}, :line(1),
    'a subset where block holding only a WhateverCode is a double closure';
throws-like ｢my $x where {* < 5} = 3｣,
    X::Syntax::Malformed, :what{.contains: 'closure'}, :line(1),
    'a variable where block holding only a WhateverCode is a double closure';
throws-like ｢class C { has $.x where {* < 5} }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'}, :line(1),
    'an attribute where block holding only a WhateverCode is a double closure';
{
    try EVAL ｢my ($a where {* < 5}, $b) = 1, 2｣;
    isa-ok $!, X::Syntax::Malformed, 'a where block in a declared signature is reported once';
}
throws-like ｢my subset S of Int where ({* < 5})｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a parenthesized subset where block holding only a WhateverCode is a double closure';
throws-like ｢sub ($a where ({* < 5})) { }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a parenthesized parameter where block holding only a WhateverCode is a double closure';
throws-like ｢my $x where ({* < 5}) = 3｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a parenthesized variable where block holding only a WhateverCode is a double closure';
throws-like ｢class C { has $.x where ({* < 5}) }｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a parenthesized attribute where block holding only a WhateverCode is a double closure';
throws-like "class D \{\n    has \$.y;\n    has \$.x where \{* < 5\};\n\}",
    X::Syntax::Malformed, :what{.contains: 'closure'}, :line(3),
    'an attribute where block on a later line is reported at that line';
todo 'the legacy frontend only looks at a leading WhateverCode', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like ｢my subset S of Int where {$_ > 0 and * < 5}｣,
    X::Syntax::Malformed, :what{.contains: 'closure'},
    'a WhateverCode right of an and in a subset where block is a double closure';
eval-lives-ok ｢my subset S of Int where * < 5; die unless 3 ~~ S｣,
    'a subset where with a bare WhateverCode is not a double closure';
eval-lives-ok ｢my subset S of Int where {$_ < 5}; die unless 3 ~~ S｣,
    'a subset where block without a WhateverCode is not a double closure';
eval-lives-ok ｢my $x where {$_ < 5} = 3｣,
    'a variable where block without a WhateverCode is not a double closure';
eval-lives-ok ｢my ($a where {$_ < 5}, $b) = 1, 2｣,
    'a where block without a WhateverCode in a declared signature is not a double closure';
