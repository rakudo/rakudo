use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 3;

if $*DISTRO.is-win {
    skip 'Test flops on Windows. See https://rt.perl.org/Ticket/Display.html?id=130042 ', 3;
    exit;
}

compile_test_lib('20-concurrent');

sub fib(int32)    returns int32 is native('./20-concurrent') { * }
sub hang_around()               is native('./20-concurrent') { * }

{
    my @results = await do for ^10 { start fib(25) }
    ok ([==] 121393, |@results), 'Correct results when running native code across threads';
}

# Now we start in a thread a C function that sleeps in a loop. A VM that handles
# this case badly might get itself blocked up waiting on the call to return prior
# to being able to do any GC.
start hang_around();

{
    my class ToUseMemory {
        has @.x = rand xx 100;
    }
    my @use-it;
    for ^5000 {
        push @use-it, ToUseMemory.new;
    }
    is @use-it.elems, 5000, 'A sleeping native call does not block running/GC in another thread';
}

is fib(30), 1346269, 'Can call native function while one in another thread is sleeping';

# vim: expandtab shiftwidth=4
