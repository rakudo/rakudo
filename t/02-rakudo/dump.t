use v6;
use Test;

plan 44;

# Undefined values DUMP as .raku
is DUMP(Mu),        Mu.raku,          'DUMP(:U) is .raku (Mu)';
is DUMP(Junction),  Junction.raku,    'DUMP(:U) is .raku (Junction)';
is DUMP(Any),       Any.raku,         'DUMP(:U) is .raku (Any)';
is DUMP(Bool),      Bool.raku,        'DUMP(:U) is .raku (Bool)';
is DUMP(Cool),      Cool.raku,        'DUMP(:U) is .raku (Cool)';
is DUMP(Str),       Str.raku,         'DUMP(:U) is .raku (Str)';
is DUMP(Int),       Int.raku,         'DUMP(:U) is .raku (Int)';
is DUMP(Num),       Num.raku,         'DUMP(:U) is .raku (Num)';
is DUMP(Rat),       Rat.raku,         'DUMP(:U) is .raku (Rat)';
is DUMP(FatRat),    FatRat.raku,      'DUMP(:U) is .raku (FatRat)';
is DUMP(Complex),   Complex.raku,     'DUMP(:U) is .raku (Complex)';
is DUMP(Duration),  Duration.raku,    'DUMP(:U) is .raku (Duration)';
is DUMP(Instant),   Instant.raku,     'DUMP(:U) is .raku (Instant)';

# Defined booleans DUMP as .Str
todo 'NYI', 2;
is DUMP(False),     False.Str,        'DUMP(Bool:D) is .Str (False)';
is DUMP(True),      True.Str,         'DUMP(Bool:D) is .Str (True)';

# Defined numbers DUMP as .raku
is DUMP(0),         (0).raku,         'DUMP(Int:D) is .raku (0)';
is DUMP(1),         (1).raku,         'DUMP(Int:D) is .raku (1)';
is DUMP(-128),      (-128).raku,      'DUMP(Int:D) is .raku (-128)';
is DUMP(123456789), (123456789).raku, 'DUMP(Int:D) is .raku (123456789)';
is DUMP(1 +< 100),  (1 +< 100).raku,  'DUMP(Int:D) is .raku (1 +< 100)';

is DUMP( 0e0),      ( 0e0).raku,      'DUMP(Num:D) is .raku (0e0)';
is DUMP(-0e0),      (-0e0).raku,      'DUMP(Num:D) is .raku (-0e0)';
is DUMP( Inf),      ( Inf).raku,      'DUMP(Num:D) is .raku (Inf)';
is DUMP(-Inf),      (-Inf).raku,      'DUMP(Num:D) is .raku (-Inf)';
is DUMP( NaN),      ( NaN).raku,      'DUMP(Num:D) is .raku (NaN)';

is DUMP( 0.0),      ( 0.0).raku,      'DUMP(Rat:D) is .raku (0.0)';
is DUMP(-0.0),      (-0.0).raku,      'DUMP(Rat:D) is .raku (-0.0)';
is DUMP( 1.1),      ( 1.1).raku,      'DUMP(Rat:D) is .raku (1.1)';
is DUMP(-1.1),      (-1.1).raku,      'DUMP(Rat:D) is .raku (-1.1)';
is DUMP( 22/7),     ( 22/7).raku,     'DUMP(Rat:D) is .raku (22/7)';
is DUMP(-22/7),     (-22/7).raku,     'DUMP(Rat:D) is .raku (-22/7)';

is DUMP(   0i),     (   0i).raku,     'DUMP(Complex:D) is .raku (0i)';
is DUMP(  -0i),     (  -0i).raku,     'DUMP(Complex:D) is .raku (-0i)';
is DUMP( 0+0i),     ( 0+0i).raku,     'DUMP(Complex:D) is .raku (0+0i)';
is DUMP( 0-0i),     ( 0-0i).raku,     'DUMP(Complex:D) is .raku (0-0i)';
is DUMP(-0+0i),     (-0+0i).raku,     'DUMP(Complex:D) is .raku (-0+0i)';
is DUMP(-0-0i),     (-0-0i).raku,     'DUMP(Complex:D) is .raku (-0-0i)';
is DUMP( 1+1i),     ( 1+1i).raku,     'DUMP(Complex:D) is .raku (1+1i)';
is DUMP( 1-1i),     ( 1-1i).raku,     'DUMP(Complex:D) is .raku (1-1i)';
is DUMP(-1+1i),     (-1+1i).raku,     'DUMP(Complex:D) is .raku (-1+1i)';
is DUMP(-1-1i),     (-1-1i).raku,     'DUMP(Complex:D) is .raku (-1-1i)';

# Variables with native primitive types dump as literals
my int $int = 42;
my num $num = 12345e0;
my str $str = 'a string';

todo 'NYI (can it even?)', 3;
is DUMP($int),      DUMP(42),         'DUMP(int) dumps as a literal';
is DUMP($num),      DUMP(12345e0),    'DUMP(num) dumps as a literal';
is DUMP($str),      DUMP('a string'), 'DUMP(str) dumps as a literal';

# vim: expandtab shiftwidth=4
