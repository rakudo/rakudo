use lib 't/04-nativecall';
use CompileTestLib;
use lib 'lib';
use NativeCall;
use Test;

plan 10;

compile_test_lib('15-rw-args');

sub SetChar(int8 is rw)           is native('./15-rw-args') { * }
sub SetShort(int16 is rw)         is native('./15-rw-args') { * }
sub SetLong(long is rw)           is native('./15-rw-args') { * }
sub SetLongLong(longlong is rw)   is native('./15-rw-args') { * }
sub SetFloat(num32 is rw)         is native('./15-rw-args') { * }
sub SetDouble(num64 is rw)        is native('./15-rw-args') { * }
sub SetUChar(uint8 is rw)         is native('./15-rw-args') { * }
sub SetUShort(uint16 is rw)       is native('./15-rw-args') { * }
sub SetULong(ulong is rw)         is native('./15-rw-args') { * }
sub SetULongLong(ulonglong is rw) is native('./15-rw-args') { * }

my int8 $c; SetChar($c);
is $c, 97, 'Perl\'s rw variable was set by C (char)';

my int16 $s; SetShort($s);
is $s, 387, 'Perl\'s rw variable was set by C (short)';

my long $l; SetLong($l);
is $l, 777, 'Perl\'s rw variable was set by C (long)';

my longlong $ll; SetLongLong($ll);
is $ll, 15324, 'Perl\'s rw variable was set by C (long long)';

my num32 $f; SetFloat($f);
is_approx $f, 6.66, 'Perl\'s rw variable was set by C (float)';

my num64 $d; SetDouble($d);
is_approx $d, 12.12, 'Perl\'s rw variable was set by C (double)';

todo 'unsigned char comes back as signed', 1;
my uint8 $uc; SetUChar($uc);
is $uc, 153, 'Perl\'s rw variable was set by C (unsigned char)';

my uint16 $us; SetUShort($us);
is $us, 387, 'Perl\'s rw variable was set by C (unsigned short)';

my ulong $ul; SetULong($ul);
is $ul, 777, 'Perl\'s rw variable was set by C (unsigned long)';

my ulonglong $ull; SetULongLong($ull);
is $ull, 15324, 'Perl\'s rw variable was set by C (unsigned long long)';

# vim:ft=perl6
