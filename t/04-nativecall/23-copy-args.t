use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 6;

compile_test_lib('23-copy-args');

class Simple is repr("CStruct") {
    has int64 $.bigint;
    has int32 $.smallint;
    has num32 $.smallnum;
}

class Point is repr("CStruct") {
    has num32 $.x;
    has num32 $.y;
};

class Size is repr("CStruct") {
    has num32 $.width;
    has num32 $.height;
}

class Rect is repr("CStruct") {
    HAS Point $.origin;
    HAS Size  $.size;

    submethod BUILD(:$x = 0e0, :$y = 0e0, :$w = 0e0; :$h = 0e0) {
	$!origin := Point.new(:$x, :$y);
	$!size   := Size.new(width => $w, height => $h);
    }
}


sub PassSimple(Simple is copy --> num32)      is native('./23-copy-args') { * }
sub ReturnsSimple(num32 --> Simple) is copy is native('./23-copy-args') { * }

sub TakeStructSimpleCopyCallback(&cb (Simple is copy), num32) is native('./23-copy-args') { * }
sub CheckReturnsStructSimpleCopy(&cb is copy (int32, num32 --> Simple), int32, num32 --> num32) is native('./23-copy-args') { * }

sub PassRect(Rect is copy --> num32)    is native('./23-copy-args') { * }

my $c = Simple.new(bigint => 1, smallint => 2, smallnum => 2e2);
is-approx PassSimple($c), 4e2,        'Perl\'s struct is correctly passed by value';

my $r = ReturnsSimple(4e-2);
isa-ok $r, Simple,                    'Return value is of the right type';
is-approx $r.smallnum, 4e-2,          'Returned the correct Struct value';

sub struct_callback_copy(Simple $struct is copy) {
    is-approx $struct.smallnum, 4e2,  'Callback got the Struct value';
}
TakeStructSimpleCopyCallback(&struct_callback_copy, 4e2);

sub struct_callback_returns(int32 $a, num32 $b --> Simple) is copy {
    return Simple.new(bigint => $a, smallnum => $b);
}
is-approx CheckReturnsStructSimpleCopy(&struct_callback_returns, 12, 2e2), 2400, 'Callback returned the Struct as copy';

my $v = Rect.new(x => 2e2, y => 2e2, w => 2e1, h => 2e1);
is-approx PassRect($v), 800, 'Can pass inlined CStruct around'; 

# vim:ft=perl6

