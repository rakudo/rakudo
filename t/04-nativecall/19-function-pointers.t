use lib 't/04-nativecall';
use CompileTestLib;
use lib 'lib';
use NativeCall;
use Test;

plan(2);

# RT #125140

compile_test_lib('19-function-pointers');

sub ForeignFunction()       returns int32 is native('./19-function-pointers') { * }
sub ReturnFunctionPointer() returns Pointer is native('./19-function-pointers') { * }

my $ptr              = ReturnFunctionPointer();
my &ReturnedFunction = nativecast(&ForeignFunction, $ptr);

is ReturnedFunction(), 42, 'Pointer cast to Perl 6 Sub';
