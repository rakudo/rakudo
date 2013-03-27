use v6;
use Test;

# Undefined values DUMP as .perl
is DUMP(Mu),        Mu.perl,          'DUMP(:U) is .perl (Mu)';
is DUMP(Junction),  Junction.perl,    'DUMP(:U) is .perl (Junction)';
is DUMP(Any),       Any.perl,         'DUMP(:U) is .perl (Any)';
is DUMP(Bool),      Bool.perl,        'DUMP(:U) is .perl (Bool)';
is DUMP(Cool),      Cool.perl,        'DUMP(:U) is .perl (Cool)';
is DUMP(Str),       Str.perl,         'DUMP(:U) is .perl (Str)';
is DUMP(Int),       Int.perl,         'DUMP(:U) is .perl (Int)';
is DUMP(Num),       Num.perl,         'DUMP(:U) is .perl (Num)';
is DUMP(Rat),       Rat.perl,         'DUMP(:U) is .perl (Rat)';
is DUMP(FatRat),    FatRat.perl,      'DUMP(:U) is .perl (FatRat)';
is DUMP(Complex),   Complex.perl,     'DUMP(:U) is .perl (Complex)';
is DUMP(Duration),  Duration.perl,    'DUMP(:U) is .perl (Duration)';
is DUMP(Instant),   Instant.perl,     'DUMP(:U) is .perl (Instant)';

# Defined booleans DUMP as .Str
is DUMP(False),     False.Str,        'DUMP(Bool:D) is .Str (False)';
is DUMP(True),      True.Str,         'DUMP(Bool:D) is .Str (True)';

# Defined numbers DUMP as .perl
is DUMP(0),         (0).perl,         'DUMP(Int:D) is .perl (0)';
is DUMP(1),         (1).perl,         'DUMP(Int:D) is .perl (1)';
is DUMP(-128),      (-128).perl,      'DUMP(Int:D) is .perl (-128)';
is DUMP(123456789), (123456789).perl, 'DUMP(Int:D) is .perl (123456789)';
is DUMP(1 +< 100),  (1 +< 100).perl,  'DUMP(Int:D) is .perl (1 +< 100)';

is DUMP( 0e0),      ( 0e0).perl,      'DUMP(Num:D) is .perl (0e0)';
is DUMP(-0e0),      (-0e0).perl,      'DUMP(Num:D) is .perl (-0e0)';
is DUMP( Inf),      ( Inf).perl,      'DUMP(Num:D) is .perl (Inf)';
is DUMP(-Inf),      (-Inf).perl,      'DUMP(Num:D) is .perl (-Inf)';
is DUMP( NaN),      ( NaN).perl,      'DUMP(Num:D) is .perl (NaN)';

is DUMP( 0.0),      ( 0.0).perl,      'DUMP(Rat:D) is .perl (0.0)';
is DUMP(-0.0),      (-0.0).perl,      'DUMP(Rat:D) is .perl (-0.0)';
is DUMP( 1.1),      ( 1.1).perl,      'DUMP(Rat:D) is .perl (1.1)';
is DUMP(-1.1),      (-1.1).perl,      'DUMP(Rat:D) is .perl (-1.1)';
is DUMP( 22/7),     ( 22/7).perl,     'DUMP(Rat:D) is .perl (22/7)';
is DUMP(-22/7),     (-22/7).perl,     'DUMP(Rat:D) is .perl (-22/7)';

is DUMP(   0i),     (   0i).perl,     'DUMP(Complex:D) is .perl (0i)';
is DUMP(  -0i),     (  -0i).perl,     'DUMP(Complex:D) is .perl (-0i)';
is DUMP( 0+0i),     ( 0+0i).perl,     'DUMP(Complex:D) is .perl (0+0i)';
is DUMP( 0-0i),     ( 0-0i).perl,     'DUMP(Complex:D) is .perl (0-0i)';
is DUMP(-0+0i),     (-0+0i).perl,     'DUMP(Complex:D) is .perl (-0+0i)';
is DUMP(-0-0i),     (-0-0i).perl,     'DUMP(Complex:D) is .perl (-0-0i)';
is DUMP( 1+1i),     ( 1+1i).perl,     'DUMP(Complex:D) is .perl (1+1i)';
is DUMP( 1-1i),     ( 1-1i).perl,     'DUMP(Complex:D) is .perl (1-1i)';
is DUMP(-1+1i),     (-1+1i).perl,     'DUMP(Complex:D) is .perl (-1+1i)';
is DUMP(-1-1i),     (-1-1i).perl,     'DUMP(Complex:D) is .perl (-1-1i)';
