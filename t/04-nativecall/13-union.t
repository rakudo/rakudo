use lib 't/04-nativecall';
use CompileTestLib;
use lib 'lib';
use NativeCall;
use Test;

plan 23;

compile_test_lib('13-union');

class Onion is repr('CUnion') {
    has long   $.l;
    has uint32 $.i;
    has uint16 $.s;
    has uint8  $.c;
}

is nativesizeof(Onion), nativesizeof(long), 'sizeof union is sizeof biggest member';

class MyStruct is repr('CStruct') {
    has long  $.long;
    has num64 $.num;
    has int8  $.byte;
    has Onion $.onion is inlined;
    has num32 $.float;

    method init() {
        $!long = 42;
        $!byte = 7;
        $!num = -3.7e0;
        $!float = 3.14e0;
    }
}

sub ReturnMyStruct()   returns MyStruct is native('./13-union') { * }
sub SizeofMyStruct()   returns int32    is native('./13-union') { * }
sub SetLongMyStruct(MyStruct)           is native('./13-union') { * }
sub SetIntMyStruct(MyStruct)            is native('./13-union') { * }
sub SetShortMyStruct(MyStruct)          is native('./13-union') { * }
sub SetCharMyStruct(MyStruct)           is native('./13-union') { * }

is nativesizeof(MyStruct), SizeofMyStruct(), 'sizeof(MyStruct)';

# Perl-side tests:
my MyStruct $obj .= new;
$obj.init;

is $obj.long,         42,     'getting long';
is_approx $obj.num,  -3.7e0,  'getting num';
is $obj.byte,         7,      'getting int8';
is_approx $obj.float, 3.14e0, 'getting num32';

# C-side tests:
my $cobj = ReturnMyStruct;

is $cobj.long,          17,     'getting long from C-created struct';
is_approx $cobj.num,    4.2e0,  'getting num from C-created struct';
is $cobj.byte,          13,     'getting int8 from C-created struct';
is_approx $cobj.float, -6.28e0, 'getting num32 from C-created struct';

SetLongMyStruct($cobj);
is $cobj.onion.l, 1 +< 30, 'long in union';

SetIntMyStruct($cobj);
is $cobj.onion.i, 1 +< 27, 'int in union';

SetShortMyStruct($cobj);
is $cobj.onion.s, 1 +< 13, 'short in union';

SetCharMyStruct($cobj);
is $cobj.onion.c, 1 +< 6,  'char in union';

class MyStruct2 is repr('CStruct') {
    has long  $.long;
    has num64 $.num;
    has int8  $.byte;
    has Onion $.onion;
    has num32 $.float;

    method init() {
        $!long = 42;
        $!byte = 7;
        $!num = -3.7e0;
        $!float = 3.14e0;
    }
}

sub ReturnMyStruct2() returns MyStruct2 is native('./13-union') { * }
sub SizeofMyStruct2() returns int32     is native('./13-union') { * }
sub SetLongMyUnion(Onion)               is native('./13-union') { * }
sub SetIntMyUnion(Onion)                is native('./13-union') { * }
sub SetShortMyUnion(Onion)              is native('./13-union') { * }
sub SetCharMyUnion(Onion)               is native('./13-union') { * }

is nativesizeof(MyStruct2), SizeofMyStruct2(), 'sizeof(MyStruct2)';

# C-side tests:
my $cobj2 = ReturnMyStruct2;

is $cobj2.long,          17,     'getting long from C-created struct';
is_approx $cobj2.num,    4.2e0,  'getting num from C-created struct';
is $cobj2.byte,          13,     'getting int8 from C-created struct';
is_approx $cobj2.float, -6.28e0, 'getting num32 from C-created struct';

my $onion = $cobj2.onion;

SetLongMyUnion($onion);
is $onion.l, 1 +< 30, 'long in union*';

SetIntMyUnion($onion);
is $onion.i, 1 +< 27, 'int in union*';

SetShortMyUnion($onion);
is $onion.s, 1 +< 13, 'short in union*';

SetCharMyUnion($onion);
is $onion.c, 1 +< 6,  'char in union*';

# vim:ft=perl6
