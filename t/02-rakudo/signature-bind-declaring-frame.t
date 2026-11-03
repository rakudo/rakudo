use Test;
use nqp;

plan 8;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

todo 'binds in the block of the try on the legacy frontend', 5 unless $rakuast;
is (try EVAL(q[try my ($a, $b) := (1, 2); $a])), 1,
    'a signature declaration bound under try binds its variables';
is (try EVAL(q[sub f { try my ($a, $b) := (1, 2); $a + $b }; f()])), 3,
    'a signature declaration bound under try in a routine binds its variables';
is (try EVAL(q[try my (::T, $x) := (Int, 5); T.^name ~ $x])), 'Int5',
    'a signature declaration starting with a type capture bound under try binds it';
is (try EVAL(q[try my ([$a, $b], $c) := ((1, 2), 3); $a + $b + $c])), 6,
    'a signature declaration starting with a sub-signature bound under try binds its variables';
is (try EVAL(q[try my ($, $b) := (1, 2); $b])), 2,
    'a signature declaration starting with an anonymous parameter bound under try binds its variables';
is (try EVAL(q[try my (::T) := \(Int); T.^name])), 'Int',
    'a signature declaration of a type capture alone bound under try binds it';
todo 'binds in the block of the try on the legacy frontend' unless $rakuast;
is (try EVAL(q[try my ([$a, $b]) := \((1, 2)); $a + $b])), 3,
    'a signature declaration of a sub-signature alone bound under try binds its variables';
todo 'binds in the frame of the thunk on the legacy frontend' unless $rakuast;
is (try EVAL(q[42 andthen (my ($a, $b) := ($_, 1)); $a])), 42,
    'a parenthesized signature declaration that binds right of andthen binds its variables';

# vim: expandtab shiftwidth=4
