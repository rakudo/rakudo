use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan(9);

compile_test_lib('21-callback-other-thread');

sub SetCallback(&cb (int32 --> int32)) is native('./21-callback-other-thread') { * }
sub CallCallback(int32 --> int32) is native('./21-callback-other-thread') { * }

sub the_callback(int32 $i --> int32) { 2 * $i }

SetCallback(&the_callback);
is CallCallback(1), 6, 'Sanity check: Calling callback on thread that set it works';

my $tap-lock = Lock.new;
my @threads = do for ^8 -> $i {
    Thread.start({
        my $result = [+] CallCallback($i) xx 100;
        $tap-lock.protect: {
            is $result, 600 * $i, "Calling callback on another thread works ($i)";
        }
    })
}
@threads>>.join;

# vim: expandtab shiftwidth=4
