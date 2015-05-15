use lib 't/04-nativecall';
use CompileTestLib;
use NativeCall;
use Test;

plan(11);

compile_test_lib('03-simple-returns');

sub ReturnInt() returns int32 is native('./03-simple-returns') { * }
is ReturnInt(), 101, 'returning int works';

sub ReturnShort() returns int16  is native('./03-simple-returns') { * }
is ReturnShort(), 102, 'returning short works';

sub ReturnByte() returns int8 is native('./03-simple-returns') { * }
is ReturnByte(), -103, 'returning char works';

sub ReturnDouble() returns num64 is native('./03-simple-returns') { * }
is_approx ReturnDouble(), 99.9e0, 'returning double works';

sub ReturnFloat() returns num32 is native('./03-simple-returns') { * }
is_approx ReturnFloat(), -4.5e0, 'returning float works';

sub ReturnString() returns Str is native('./03-simple-returns') { * }
is ReturnString(), "epic cuteness", 'returning string works';

sub ReturnNullString returns Str is native('./03-simple-returns') { * }
nok ReturnNullString().defined, 'returning null string pointer';

sub ReturnInt64() returns int64 is native('./03-simple-returns') { * }
is ReturnInt64(), 0xFFFFFFFFFF, 'returning int64 works';

sub ReturnUint8() returns uint8 is native('./03-simple-returns') { * }
is ReturnUint8(), 0xFE, 'returning uint8 works';

sub ReturnUint16() returns uint16 is native('./03-simple-returns') { * }
is ReturnUint16(), 0xFFFE, 'returning uint16 works';

sub ReturnUint32() returns uint32 is native('./03-simple-returns') { * }
is ReturnUint32(), 0xFFFFFFFE, 'returning uint32 works';
