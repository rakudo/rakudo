use v6;

# Simple type conversions


say '1..6';

my Int $int;
my Str $str;
my Num $num;

$str = '14';
$int = 14;

($str == $int) && say 'ok 1';
($str eq $int) && say 'ok 2';

($str == 14) && say 'ok 3';
($str eq 14) && say 'ok 4';

# From http://www.rakudo.org/2008/06/mixins-generic-routines-and-en.html
sub typeof(::T $x) { return T }
(typeof(42) eq 'Int()') && say 'ok 5';
(typeof('42') eq 'Str()') && say 'ok 6';

# Stuff that goes crashy-crashy right now
#$num = 0;
#my Array of Int $intarray;
# ($num == $int) && say 'ok 2';
# ($str == $num) && say 'ok 3';
