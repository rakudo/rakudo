use Test;
use nqp;

plan 11;

is (42 andthen sub ($x) { $x + 1 }), 43,
    'andthen calls a sub literal with its defined left side';
is (Any orelse sub ($x) { "got " ~ $x.raku }), 'got Any',
    'orelse calls a sub literal with its undefined left side';
is (Any notandthen sub ($x) { "got " ~ $x.raku }), 'got Any',
    'notandthen calls a sub literal with its undefined left side';
is (42 andthen *.succ andthen sub ($x) { $x * 2 }), 86,
    'chained andthen calls a sub literal with the value before it';
is ([andthen] 42, sub ($x) { $x + 1 }), 43,
    '[andthen] calls a sub literal with the value before it';
is (sub ($x) { $x + 1 } Randthen 42), 43,
    'Randthen calls a sub literal left side with its defined right side';
is-deeply ((42,) Zandthen (sub ($x) { $x + 1 },)).List, (43,),
    'Zandthen calls a sub literal element with its left element';
todo 'the legacy frontend returns a parenthesized sub literal uncalled', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is (42 andthen (sub ($x) { $x + 1 })), 43,
    'andthen calls a parenthesized sub literal with its defined left side';

todo 'the legacy frontend calls a regex declarator with the left side', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is (try ("abc" andthen regex { b }).^name), 'Regex',
    'andthen returns a regex declarator on its right, as it does a regex literal';
is ~("abc" andthen m/b/), 'b',
    'andthen matches a regex literal on its right against its defined left side';
is (42 andthen try { $_ + 1 }), 43,
    'andthen runs a try on its right with its defined left side as the topic';
