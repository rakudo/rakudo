use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan(7);

compile_test_lib('10-cglobals');

my $GlobalInt := cglobal('./10-cglobals', 'GlobalInt', int32);
is $GlobalInt, 101, 'global int works';

my $GlobalShort := cglobal('./10-cglobals', 'GlobalShort', int16);
is $GlobalShort, 102, 'global short works';

my $GlobalByte := cglobal('./10-cglobals', 'GlobalByte', int8);
is $GlobalByte, -103, 'global char works';

my $GlobalDouble := cglobal('./10-cglobals', 'GlobalDouble', num64);
is-approx $GlobalDouble, 99.9e0, 'global double works';

my $GlobalFloat := cglobal('./10-cglobals', 'GlobalFloat', num32);
is-approx $GlobalFloat, -4.5e0, 'global float works';

my $GlobalString := cglobal('./10-cglobals', 'GlobalString', str);
is $GlobalString, "epic cuteness", 'global string works';

my $GlobalNullString := cglobal('./10-cglobals', 'GlobalNullString', str);
nok $GlobalNullString.defined, 'global null string pointer';

# vim: expandtab shiftwidth=4
