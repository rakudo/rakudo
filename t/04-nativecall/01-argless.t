use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 12;

compile_test_lib('01-argless');

sub Nothing() is native('./01-argless') { * }
sub Argless() is native('./01-argless') returns int32 { * }
sub ArglessChar() is native('./01-argless') returns int8 { * }
sub ArglessLongLong() is native('./01-argless') returns int64 { * }
sub ArglessPointer() is native('./01-argless') returns Pointer[int32] { * }
sub short()   is native('./01-argless') returns int32 is symbol('long_and_complicated_name') { * }

Nothing() for ^2;

pass 'survived the call';

is Argless(), 2, 'called argless function' for ^2;
is ArglessChar(), 2, 'called argless function' for ^2;
is ArglessLongLong(), 2, 'called argless function' for ^2;
is ArglessPointer().deref, 2, 'called argless function' for ^2;

is short(), 3, 'called long_and_complicated_name';

sub test-native-closure() {
    my sub Argless() is native('./01-argless') returns int32 { * }
    is Argless(), 2, 'called argless closure';
}

test-native-closure();
test-native-closure(); # again cause we may have created an optimized version to run
