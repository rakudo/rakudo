use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan(20);

compile_test_lib('03-simple-returns');

sub ReturnInt() returns int32 is native('./03-simple-returns') { * }
is ReturnInt(), 101, 'returning int works';
is ReturnInt(), 101, 'returning int works';

sub ReturnNegInt() returns int32 is native('./03-simple-returns') { * }
is ReturnNegInt(), -101, 'returning negative int works';
is ReturnNegInt(), -101, 'returning negative int works';

sub ReturnShort() returns int16  is native('./03-simple-returns') { * }
is ReturnShort(), 102, 'returning short works';
is ReturnShort(), 102, 'returning short works';

sub ReturnNegShort() returns int16  is native('./03-simple-returns') { * }
is ReturnNegShort(), -102, 'returning negative short works';
is ReturnNegShort(), -102, 'returning negative short works';

sub ReturnByte() returns int8 is native('./03-simple-returns') { * }
is ReturnByte(), -103, 'returning char works';
is ReturnByte(), -103, 'returning char works';

sub ReturnDouble() returns num64 is native('./03-simple-returns') { * }
is-approx ReturnDouble(), 99.9e0, 'returning double works';

sub ReturnFloat() returns num32 is native('./03-simple-returns') { * }
is-approx ReturnFloat(), -4.5e0, 'returning float works';

sub ReturnString() returns Str is native('./03-simple-returns') { * }
is ReturnString(), "epic cuteness", 'returning string works';

sub ReturnNullString returns Str is native('./03-simple-returns') { * }
nok ReturnNullString().defined, 'returning null string pointer';

sub ReturnInt64() returns int64 is native('./03-simple-returns') { * }
is ReturnInt64(), 0xFFFFFFFFFF, 'returning int64 works';

sub ReturnNegInt64() returns int64 is native('./03-simple-returns') { * }
is ReturnNegInt64(), -0xFFFFFFFFFF, 'returning negative int64 works';
is ReturnNegInt64(), -0xFFFFFFFFFF, 'returning negative int64 works';

sub ReturnUint8() returns uint8 is native('./03-simple-returns') { * }
is ReturnUint8(), 0xFE, 'returning uint8 works';

sub ReturnUint16() returns uint16 is native('./03-simple-returns') { * }
is ReturnUint16(), 0xFFFE, 'returning uint16 works';

sub ReturnUint32() returns uint32 is native('./03-simple-returns') { * }
is ReturnUint32(), 0xFFFFFFFE, 'returning uint32 works';

# vim: expandtab shiftwidth=4
