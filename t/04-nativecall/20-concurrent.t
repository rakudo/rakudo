use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 1;

compile_test_lib('20-concurrent');

sub fib(int32)    returns int32 is native('./20-concurrent') { * }
sub hang_around()               is native('./20-concurrent') { * }

my @results = await do for ^10 { start fib(25) }
ok ([==] 121393, |@results), 'Correct results when running native code across threads';

# vim:ft=perl6
