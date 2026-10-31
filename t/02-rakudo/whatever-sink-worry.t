use Test;
use nqp;
use MONKEY-SEE-NO-EVAL;

plan 24;

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    throws-like ｢use fatal; my $x; $x R//= *; 1｣, X::AdHoc,
        :payload(/'Useless use of $x R//= * in sink context'/),
        'a sunk R//= that primes draws a worry';
}
else {
    skip 'the legacy frontend cannot compile a reversed //=', 1;
}

todo 'the legacy frontend does not worry about every sunk WhateverCode', 14
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like ｢use fatal; my @a = 1, 2; @a Z= *; 1｣, X::AdHoc,
    :payload(/'Useless use of @a Z= * in sink context'/),
    'a sunk Z= that primes draws a worry';
throws-like ｢use fatal; my @a = 1, 2; @a »=» *; 1｣, X::AdHoc,
    :payload(/'Useless use of @a »=» * in sink context'/),
    'a sunk »=» that primes draws a worry';
throws-like ｢use fatal; my $x; $x [R//]= *; 1｣, X::AdHoc,
    :payload(/'Useless use of $x [R//]= * in sink context'/),
    'a sunk [R//]= that primes draws a worry';
throws-like ｢use fatal; my $x = 1; $x += *; 1｣, X::AdHoc,
    :payload(/'Useless use of $x += * in sink context'/),
    'a sunk += that primes draws a worry';
throws-like ｢use fatal; my $x = 1; $x [+]= *; 1｣, X::AdHoc,
    :payload(/'Useless use of $x [+]= * in sink context'/),
    'a sunk [+]= that primes draws a worry';
throws-like ｢use fatal; * ~~ Int; 1｣, X::AdHoc,
    :payload(/'Useless use of * ~~ Int in sink context'/),
    'a sunk ~~ that primes draws a worry';
throws-like ｢use fatal; my $x; $x // *.succ; 1｣, X::AdHoc,
    :payload(/'Useless use of *.succ in sink context'/),
    'a sunk WhateverCode right of // draws a worry';
throws-like ｢use fatal; *.succ; 1｣, X::AdHoc,
    :payload(/'Useless use of *.succ in sink context'/),
    'a sunk method call that primes draws a worry';
throws-like ｢use fatal; *[0]; 1｣, X::AdHoc,
    :payload(/'Useless use of *[0] in sink context'/),
    'a sunk subscript that primes draws a worry';
throws-like ｢use fatal; ++*; 1｣, X::AdHoc,
    :payload(/'Useless use of ++* in sink context'/),
    'a sunk ++ prefix that primes draws a worry';
throws-like ｢use fatal; *++; 1｣, X::AdHoc,
    :payload(/'Useless use of *++ in sink context'/),
    'a sunk ++ postfix that primes draws a worry';
throws-like ｢use fatal; for 1 { *.succ }; 1｣, X::AdHoc,
    :payload(/'Useless use of *.succ in sink context'/),
    'a WhateverCode that primes at the end of a sunk loop body draws a worry';
throws-like ｢use fatal; my @a = 1, 2; *.succ for @a; 1｣, X::AdHoc,
    :payload(/'Useless use of *.succ in sink context'/),
    'a WhateverCode that a for modifier repeats draws a worry';
throws-like ｢use fatal; my $x = 5; $x andthen * + 1; 1｣, X::AdHoc,
    :payload(/'Useless use of * + 1 in sink context'/),
    'a sunk pure WhateverCode that andthen calls draws a worry';

lives-ok { EVAL ｢use fatal; my $x = 5; $x andthen *.succ; 1｣ },
    'a sunk WhateverCode that andthen calls draws no worry';
lives-ok { EVAL ｢use fatal; my $x; $x orelse *.defined; 1｣ },
    'a sunk WhateverCode that orelse calls draws no worry';
lives-ok { EVAL ｢use fatal; my $x; $x notandthen *.defined; 1｣ },
    'a sunk WhateverCode that notandthen calls draws no worry';
lives-ok { EVAL ｢use fatal; my $x = 5; $x andthen *.succ andthen *.pred; 1｣ },
    'a sunk WhateverCode that a chained andthen calls draws no worry';
lives-ok { EVAL ｢use fatal; my $x = 5; $x [andthen] *.succ; 1｣ },
    'a sunk WhateverCode that a bracketed andthen calls draws no worry';
lives-ok { EVAL ｢use fatal; my $x; $x //= *; 1｣ },
    'a sunk //= that assigns a Whatever draws no worry';
lives-ok { EVAL ｢use fatal; my @a = 1, 2; @a[*-1] += 1; 1｣ },
    'a sunk compound assignment to a WhateverCode subscript draws no worry';
lives-ok { EVAL ｢use fatal; my $x = 1; my &f = { $x += * }; 1｣ },
    'a WhateverCode a block returns draws no worry';
lives-ok { EVAL ｢use fatal; my $x; $x = * + 1; 1｣ },
    'a WhateverCode assigned to a variable draws no worry';
