use lib 't/04-nativecall';
use CompileTestLib;
use NativeCall;
use Test;

plan 3;

compile_test_lib('01-argless');

sub Nothing() is native('./01-argless') { * }
sub Argless() is native('./01-argless') returns int32 { * }
sub short()   is native('./01-argless') returns int32 is symbol('long_and_complicated_name') { * }

Nothing();

pass 'survived the call';

is Argless(), 2, 'called argless function';

is short(), 3, 'called long_and_complicated_name';
