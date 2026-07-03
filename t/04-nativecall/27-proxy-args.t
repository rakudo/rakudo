use lib <t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 6;

compile_test_lib('27-proxy-args');

class Handle is repr('CPointer') { }

sub TakePointerReturnInt(Handle --> int32) is native('./27-proxy-args') { * }
sub TakePointerReturnDouble(Handle --> num64) is native('./27-proxy-args') { * }
sub TakePointerReturnString(Handle --> Str) is native('./27-proxy-args') { * }

# A CArray element is a Proxy, which sends the call through the proxy-reader
# code the dispatcher compiles at runtime. A dispatch resumed from inside a
# reader boxes its native result using the reader frame's HLL, so a reader
# without the Raku HLL handed back BOOT types (a BOOTInt for an int return)
# on which ordinary method calls like .defined died.
my $handles = CArray[Handle].new;
$handles[0] = Handle;

my $int-result = TakePointerReturnInt($handles[0]);
isa-ok $int-result, Int, 'int return through a Proxy argument boxes to Int';
is $int-result, 42, 'the boxed int return holds its value';

my $num-result = TakePointerReturnDouble($handles[0]);
isa-ok $num-result, Num, 'num return through a Proxy argument boxes to Num';
is $num-result, 1.5e0, 'the boxed num return holds its value';

my $str-result = TakePointerReturnString($handles[0]);
isa-ok $str-result, Str, 'str return through a Proxy argument boxes to Str';
is $str-result, 'hi', 'the boxed str return holds its value';

# vim: expandtab shiftwidth=4
