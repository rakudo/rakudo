use lib 't/04-nativecall';
use CompileTestLib;
use lib 'lib';
use NativeCall;
use Test;

plan 10;

compile_test_lib('04-pointers');

sub ReturnSomePointer()               returns OpaquePointer is native("./04-pointers") { * }
sub CompareSomePointer(OpaquePointer) returns int32         is native("./04-pointers") { * }

my $x     = ReturnSomePointer();
my int $a = 4321;

ok CompareSomePointer($x), 'Got passed back the pointer I returned';
ok $x,     'Non-NULL pointer is trueish';
ok $x.Int, 'Calling .Int on non-NULL pointer is trueish';
ok +$x,    'Calling prefix:<+> on non-NULL pointer is trueish';
is +$x.perl.EVAL, +$x, 'OpaquePointer roundtrips okay using .perl and EVAL';
is OpaquePointer.new.gist,       'OpaquePointer<NULL>',   'OpaquePointer.new gistifies to "OpaquePointer<NULL>"';
is OpaquePointer.new(0).gist,    'OpaquePointer<NULL>',   'OpaquePointer.new(0) gistifies to "OpaquePointer<NULL>"';
is OpaquePointer.new(1234).gist, 'OpaquePointer<0x4d2>',  'OpaquePointer.new(1234) gistifies to "OpaquePointer<0x4d2>"';
is OpaquePointer.new($a).gist,   'OpaquePointer<0x10e1>', 'OpaquePointer.new accepts a native int too';
is OpaquePointer.gist,           '(OpaquePointer)',       'The OpaquePointer type object gistifies ot "OpaquePointer"';
