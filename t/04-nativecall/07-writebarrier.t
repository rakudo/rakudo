use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 7;

compile_test_lib('07-writebarrier');

class IntPtr is repr('CPointer') {
    sub _deref(IntPtr $x) returns long is native('./07-writebarrier') { * }
    method deref() { return _deref(self); }
}

class Structy is repr('CStruct') {
    has IntPtr $.ptr;

    method set(\ptr) {
        $!ptr := ptr;
    }
}

sub make_ptr() returns IntPtr  is native('./07-writebarrier') { * }
sub array_twiddle(CArray[IntPtr] $a) is native('./07-writebarrier') { * }
sub struct_twiddle(Structy $s) is native('./07-writebarrier') { * }
sub dummy(CArray[Pointer] $a) is native('./07-writebarrier') { * }
sub save_ref(Structy $s) is native('./07-writebarrier') { * }
sub atadistance() is native('./07-writebarrier') { * }

my Structy $s .= new;
$s.set(make_ptr);

is $s.ptr.deref, 32, 'pointer in struct before twiddle';
struct_twiddle($s);
is $s.ptr.deref, 9, 'pointer in struct after twiddle';

my @arr := CArray[IntPtr].new;
@arr[0] = make_ptr;
@arr[1] = make_ptr;
@arr[2] = make_ptr;
array_twiddle(@arr);
is @arr[0].deref, 1, 'array element 1 after twiddle';
is @arr[1].deref, 2, 'array element 2 after twiddle';
is @arr[2].deref, 3, 'array element 3 after twiddle';

dummy(CArray);
pass 'ignore NULL arguments';

save_ref($s);
atadistance();
refresh($s);
is($s.ptr.deref, 42, 'struct value after refresh');

# vim: expandtab shiftwidth=4
