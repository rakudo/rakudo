use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 5;

compile_test_lib('01-argless');

sub Nothing() is native('./01-argless') { * }
sub Argless() is native('./01-argless') returns int32 { * }
sub short()   is native('./01-argless') returns int32 is symbol('long_and_complicated_name') { * }

Nothing();

pass 'survived the call';

is Argless(), 2, 'called argless function';

is short(), 3, 'called long_and_complicated_name';

sub test-native-closure() {
    my sub Argless() is native('./01-argless') returns int32 { * }
    is Argless(), 2, 'called argless closure';
}

test-native-closure();
test-native-closure(); # again cause we may have created an optimized version to run
