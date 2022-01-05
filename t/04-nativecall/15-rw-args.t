use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 22;

compile_test_lib('15-rw-args');

sub SetChar(int8 is rw)            is native('./15-rw-args') { * }
sub PassChar(int8 is rw)           returns int8 is native('./15-rw-args') { * }
sub SetShort(int16 is rw)          is native('./15-rw-args') { * }
sub PassShort(int16 is rw)         returns int16 is native('./15-rw-args') { * }
sub SetLong(long is rw)            is native('./15-rw-args') { * }
sub PassLong(long is rw)           returns long is native('./15-rw-args') { * }
sub SetLongLong(longlong is rw)    is native('./15-rw-args') { * }
sub PassLongLong(longlong is rw)   returns longlong is native('./15-rw-args') { * }
sub SetFloat(num32 is rw)          is native('./15-rw-args') { * }
sub PassFloat(num32 is rw)         returns num32 is native('./15-rw-args') { * }
sub SetDouble(num64 is rw)         is native('./15-rw-args') { * }
sub PassDouble(num64 is rw)        returns num64 is native('./15-rw-args') { * }
sub SetUChar(uint8 is rw)          is native('./15-rw-args') { * }
sub PassUChar(uint8 is rw)         returns uint8 is native('./15-rw-args') { * }
sub SetUShort(uint16 is rw)        is native('./15-rw-args') { * }
sub PassUShort(uint16 is rw)       returns uint16 is native('./15-rw-args') { * }
sub SetULong(ulong is rw)          is native('./15-rw-args') { * }
sub PassULong(ulong is rw)         returns ulong is native('./15-rw-args') { * }
sub SetULongLong(ulonglong is rw)  is native('./15-rw-args') { * }
sub PassULongLong(ulonglong is rw) returns ulonglong is native('./15-rw-args') { * }
sub SetPtrToPtr(Pointer is rw) returns int32 is native('./15-rw-args') { * }

my int8 $c; SetChar($c);
is $c, 97, 'Perl\'s rw variable was set by C (char)';
is PassChar($c), 97, 'Perl\'s rw variable was passed and returned by C (char)';

my int16 $s; SetShort($s);
is $s, 387, 'Perl\'s rw variable was set by C (short)';
is PassShort($s), 387, 'Perl\'s rw variable was passed and returned by C (short)';

my long $l; SetLong($l);
is $l, 777, 'Perl\'s rw variable was set by C (long)';
is PassLong($l), 777, 'Perl\'s rw variable was passed and returned by C (long)';

my longlong $ll; SetLongLong($ll);
is $ll, 15324, 'Perl\'s rw variable was set by C (long long)';
is PassLongLong($ll), 15324, 'Perl\'s rw variable was passed and returned by C (longlong)';

my num32 $f; SetFloat($f);
is-approx $f, 6.66, 'Perl\'s rw variable was set by C (float)';
is-approx PassFloat($f), 6.66, 'Perl\'s rw variable was passed and returned by C (float)';

my num64 $d; SetDouble($d);
is-approx $d, 12.12, 'Perl\'s rw variable was set by C (double)';
is PassDouble($d), 12.12, 'Perl\'s rw variable was passed and returned by C (double)';

my uint8 $uc; SetUChar($uc);
is $uc, 153, 'Perl\'s rw variable was set by C (unsigned char)';
is PassUChar($uc), 153, 'Perl\'s rw variable was passed and returned by C (unsigned char)';

my uint16 $us; SetUShort($us);
is $us, 387, 'Perl\'s rw variable was set by C (unsigned short)';
is PassUShort($us), 387, 'Perl\'s rw variable was passed and returned by C (unsigned short)';

my ulong $ul; SetULong($ul);
is $ul, 777, 'Perl\'s rw variable was set by C (unsigned long)';
is PassULong($ul), 777, 'Perl\'s rw variable was passed and returned by C (unsigned long)';

my ulonglong $ull; SetULongLong($ull);
is $ull, 15324, 'Perl\'s rw variable was set by C (unsigned long long)';
is PassULongLong($ull), 15324, 'Perl\'s rw variable was passed and returned by C (unsigned long long)';

my Pointer $ptr .= new;
ok SetPtrToPtr($ptr), 'Can pass an instantiated pointer with rw-trait to C';
is +$ptr, 42, 'Perl\'s rw variable was set by C (pointer)';

# vim: expandtab shiftwidth=4
