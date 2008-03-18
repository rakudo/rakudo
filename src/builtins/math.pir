## $Id$

=head1 NAME

src/builtins/math.pir - Perl6 math functions

=head1 Math::Basic

=head2 Functions

=over 4

=cut

## TODO: figure out what to get working, in order to uncomment the following
## .namespace [ 'Math::Basic' ]


=item abs

 our Num multi Num::abs ( Num $x )
 our Num multi Math::Basic::abs ( Num $x )

Absolute Value.

=cut

.sub 'abs'
    .param pmc a
    $P0 = abs a
    .return ($P0)
.end


=item floor

 our Int multi Num::floor ( Num $x )

Returns the highest integer not greater than $x.

=cut

.sub 'floor'
    .param num a
    floor a
    .return (a)
.end


=item ceiling

 our Int multi Num::ceiling ( Num $x )
 &Num::ceil ::= &Num::ceiling;

Returns the lowest integer not less than $x.

=cut

.sub 'ceiling'
    .param num a
    ceil a
    .return (a)
.end


=item round

 our Int multi Num::round ( Num $x )
 our Int multi Int ( Num $x )

Returns the nearest integer to $x.  The algorithm is floor($x + 0.5).
(Other rounding algorithms will be given extended names beginning with "round".)

=cut

.sub 'round'
    .param num a
    a += 0.5
    $N0 = floor a
    .return (a)
.end


=item sign

 our Int multi Num::sign ( Num  $x )
 our Int multi Math::Basic::sign ( Num $x )
   if !defined($x) { return undef };
   if $x < 0       { return -1    };
   if $x > 0       { return  1    };
   if $x == 0      { return  0    };
   fail;
 }

or more succinctly:

 our Int multi Math::Basic::sign ( Num $x )
   $x <=> 0;
 }

=cut

.sub 'sign'
    .param pmc a
    $I0 = cmp_num a, 0
    .return ($I0)
.end


=item sqrt

 our Num     multi Num::sqrt ( Num  $x )
 our Complex multi Complex::sqrt ( Num  $x )
 our Complex multi Complex::sqrt ( Complex  $x )
 our Num     multi Math::Basic::sqrt ( Num $x )

C<$x ** 0.5>

=cut

.sub 'sqrt'
    .param num a
    a = sqrt a
    .return (a)
.end


=item truncate

 our Int multi Num::truncate ( Num $x )
 our &Num::int ::= &Num::truncate;

Returns the closest integer to $x whose absolute value is not greater
than the absolute value of $x.  (In other words, just chuck any
fractional part.)  This is the default rounding function used by an
C<int()> cast, for historic reasons.  But see Int constructor above
for a rounded version.

=cut

.sub 'truncate'
    .param num a
    eq a, 0, return
    lt a, 0, under
    floor a
    goto return
  under:
    ceil a
  return:
    .return (a)
.end

.sub 'int'
    .param num a
    .return 'truncate'(a)
.end


=item exp

 our Num multi Num::exp         ( Num $exponent: Num :$base = Num::e )
 our Num multi Math::Basic::exp ( Num $exponent, Num :$base = Num::e )

Performs similar to C<$base ** $exponent>. C<$base> defaults to the
constant I<e>.

=cut

.sub 'exp'
    .param num a
    a = exp a
    .return (a)
.end


=item log

 our Num multi Num::log         ( Num $x: Num :$base )
 our Num multi Math::Basic::log ( Num $x, Num :$base )

Logarithm of base C<$base>, default Natural. Calling with C<$x == 0> is an
error.

=cut

.sub 'log'
    .param num a
    $N0 = ln a
    .return ($N0)
.end


=item log10

 &log10 := &log.assuming:base(10);

=cut

.sub 'log10'
    .param num a
    $N0 = log10 a
    .return ($N0)
.end


=item e

 constant Num Num::e = exp(1);

=cut

.sub 'e'
    $N0 = exp 1
    .return ($N0)
.end


=item pi

 constant Num Num::pi = atan(1,1) * 4;
 constant Int Int::pi = 3;

=cut

.sub 'pi'
    $N0 = atan 1
    $N0 *= 4
    .return ($N0)
.end


=item radcalc

=cut

.include 'library/dumper.pir' # XXX

.sub 'radcalc'
    .param int radix
    .param string intpart
    .param string fracpart     :optional
    .param int    has_fracpart :opt_flag
    .param num    base         :optional
    .param int    has_base     :opt_flag
    .param num    exp          :optional
    .param int    has_exp      :opt_flag
    .local num    result, fracdivisor, magnitude
    .local pmc     iter

    if radix <= 1 goto err_range
    if radix > 36 goto err_range

    result       = 0.0
    fracdivisor = 1.0

    $P0 = split '', intpart
    iter = new 'Iterator', $P0

  lp1: # Accumulate over decimal part
    unless iter goto ex1
    $S0 = shift iter
    $S0 = downcase $S0
    if $S0 == "_" goto lp1
    $I0 = index "0123456789abcdefghijklmnopqrstuvwxyz", $S0
    if $I0 == -1 goto err_char
    $N0 = $I0
    result *= radix
    result += $N0
    goto lp1

  ex1:
    unless has_fracpart goto nofracpart
    $I0 = length fracpart
    unless $I0 goto nofracpart
    $P0 = split '', fracpart
    $P99 = shift $P0                             # remove the radix point

  lp2: # Accumulate over fractional part, keep length
    unless iter goto ex2
    $S0 = shift iter
    $S0 = downcase $S0
    if $S0 == "_" goto lp2
    $I0 = index "0123456789abcdefghijklmnopqrstuvwxyz", $S0
    if $I0 == -1 goto err_char
    $N0 = $I0

    result *= radix
    result += $N0
    fracdivisor *= radix
    goto lp2

  ex2:
    result /= fracdivisor

  nofracpart:
    unless has_base goto ret
    magnitude = base ** exp
    result *= magnitude
  ret:
    .return (result)

  err_range:
    die "radix out of range (2-36)"
  err_char:
    $S0 = concat "unrecognized character: ", $S0
    die $S0
.end


=back

=head2 TODO: Functions

=over 4

=item rand

 our Num multi Math::Basic::rand ( Num $x = 1 )

Pseudo random number in range C<< 0 ..^ $x >>.  That is, C<0> is theoretically possible,
while C<$x> is not.

=item srand

 multi Math::Basic::srand ( Num $seed = default_seed_algorithm())

Seed the generator C<rand> uses. C<$seed> defaults to some combination
of various platform dependent characteristics to yield a non-deterministic seed.
Note that you get one C<srand()> for free when you start a Perl program, so
you I<must> call C<srand()> yourself if you wish to specify a deterministic seed
(or if you wish to be differently nondeterministic).

=item i

 constant Complex Complex::i = Complex::sqrt(-1);

=back

=cut


=head1 Math::Trig

=head2 Functions

=over 4

=cut


## TODO: figure out what to get working, in order to uncomment the following
## .namespace [ 'Math'; 'Trig' ]


=item I<Standard Trig Functions>

 our Num multi Num::func ( Num  $x            : :$base = 'radians' )
 our Num multi Math::Trig::func ( Num $x, :$base = 'radians' )

where I<func> is one of:
sin, cos, tan, asin, acos, atan, sec, cosec, cotan, asec, acosec,
acotan, sinh, cosh, tanh, asinh, acosh, atanh, sech, cosech, cotanh,
asech, acosech, acotanh.

Performs the various trigonmetric functions.

Option C<:$base> is used to declare how you measure your angles.
Given the value of an arc representing a single full revolution.

 $base  	Result
 ----   	-------
 /:i ^r/	Radians  (2*pi)
 /:i ^d/	Degrees  (360)
 /:i ^g/	Gradians (400)
 Num    	Units of 1 revolution.

Note that module currying can be used within a lexical scope to specify
a consistent base so you don't have to supply it with every call:

 my module Trig ::= Math::Trig.assuming(:base<degrees>);

This overrides the default of "radians".

B<NOTE:> These only work with radians so far.

=item atan

 our Num multi Math::Trig::atan2 ( Num $y, Num $x = 1 : Num :$base )

This second form of C<atan> computes the arctangent of $y/$x, and takes
the quadrant into account. Otherwise behaves as other trigonometric functions.

[Note: changed atan back to atan2, or the default $x = 1 will confuse MMD.
The other alternative would be to remove the default. --law]

=cut


## XXX: conjectural as Rakudo subs do not support adverbs yet
.sub 'sin'
    .param num a
    .param string base  :optional :named('base')
    .param int has_base :opt_flag
    .local num converter
    converter = 1
    unless has_base goto doit
    unless base goto doit
    $S0 = base
    downcase $S0
    $S1 = substr $S0, 0, 1
    $I0 = index 'rdg123456789', $S1
    if $I0 == -1 goto err_unrecognized_base
    if $I0 == 0 goto doit
    converter = atan 1
    if $I0 == 1 goto deg
    if $I0 == 2 goto grad
  user_defined:
    $N0 = $S0
    $N0 /= 8
    converter /= $N0
    goto doit
  deg:
    converter /= 45
    goto doit
  grad:
    converter /= 50
  doit:
    a *= converter

  body:
    $N0 = sin a
    .return ($N0)

  err_unrecognized_base:
    $S1 = "sin: unrecognized base '"
    $S1 .= $S0
    $S1 .= "'"
    .return 'die'($S1)
.end


.sub 'cos'
    .param num a
    $N0 = cos a
    .return ($N0)
.end


.sub 'tan'
    .param num a
    $N0 = tan a
    .return ($N0)
.end


.sub 'asin'
    .param num a
    $N0 = asin a
    .return ($N0)
.end


.sub 'acos'
    .param num a
    $N0 = acos a
    .return ($N0)
.end


.sub 'atan'
    .param num a
    $N0 = atan a
    .return ($N0)
.end


.sub 'atan2'
    .param num a
    .param num b
    $N0 = atan a, b
    .return (N0)
.end


.sub 'cosec'
    .param num a
    $N0 = sin a
    $N0 = 1 / $N0
    .return ($N0)
.end


.sub 'sec'
    .param num a
    $N0 = sec a
    .return ($N0)
.end


.sub 'cotan'
    .param num a
    $N0 = tan a
    $N0 = 1 / $N0
    .return ($N0)
.end


## acosec(A) = atan(A / sqrt(A * A - 1)) + (sign(A) - 1) * (2 * atan(1))
.sub 'acosec'
    .param num a
    $N0 = a ** 2
    $N0 -= 1
    $N0 = a / $N0
    $N0 = atan $N0
    $N1 = 'sign'(a)
    $N1 -= 1
    $N1 *= 2
    $N2 = atan 1
    $N1 *= $N2
    $N0 += $N1
    .return ($N0)
.end


## asec(A) = atan(A / sqrt(A * A - 1)) + sign(A - 1) * (2 * atan(1))
.sub 'asec'
    .param num a
    $N0 = a ** 2
    $N0 -= 1
    $N0 = a / $N0
    $N0 = atan $N0
    $N1 = a - 1
    $N1 = 'sign'($N1)
    $N1 *= 2
    $N2 = atan 1
    $N1 *= $N2
    $N0 += $N1
    .return ($N0)
.end


## acotan(A) = atan(A) + 2 * atan(1)
.sub 'acotan'
    .param num a
    $N0 = atan 1
    $N0 *= 2
    $N1 = atan a
    $N0 += $N1
    .return ($N0)
.end


.sub 'sinh'
    .param num a
    $N0 = sinh a
    .return ($N0)
.end


.sub 'cosh'
    .param num a
    $N0 = cosh a
    .return ($N0)
.end


.sub 'tanh'
    .param num a
    $N0 = tanh a
    .return ($N0)
.end


## asinh(A)   = ln(A + sqrt(A * A + 1))
.sub 'asinh'
    .param num a
    $N0 = a * a
    $N0 += 1
    $N0 = sqrt $N0
    a += $N0
    a = ln a
    .return (a)
.end


## acosh(A)   = ln(A + sqrt(A * A - 1))
.sub 'acosh'
    .param num a
    $N0 = a * a
    $N0 -= 1
    $N0 = sqrt $N0
    a += $N0
    a = ln a
    .return (a)
.end


## atanh(A)   = ln((1 + A) / (1 - A)) / 2
.sub 'atanh'
    .param num a
    $N0 = 1 - a
    a += 1
    a /= $N0
    a = ln a
    a /= 2
    .return (a)
.end


## cosech(A)  = 1 / sinh(A)
.sub 'cosech'
    .param num a
    a = sinh a
    a = 1 / a
    .return (a)
.end


.sub 'sech'
    .param num a
    $N0 = sech a
    .return ($N0)
.end


## cotanh(A)  = 1 / tanh(A)
.sub 'cotanh'
    .param num a
    a = tanh a
    a = 1 / a
    .return (a)
.end


## acosech(A) = ln((sign(A) * sqrt(A * A + 1) + 1) / A)
.sub 'acosech'
    .param num a
    $N0 = a * a
    $N0 += 1
    $N0 = sqrt $N0
    $N1 = 'sign'(a)
    $N0 *= $N1
    $N0 += 1
    $N0 = ln $N0
    $N0 /= a
    .return ($N0)
.end


## asech(A)   = ln((sqrt(-A * A + 1) + 1) / A)
.sub 'asech'
    .param num a
    $N0 = neg a
    $N0 *= a
    $N0 += 1
    $N0 = sqrt $N0
    $N0 += 1
    $N0 /= a
    $N0 = ln $N0
    .return ($N0)
.end


## acotanh(A) = ln((A + 1) / (A - 1)) / 2
.sub 'acotanh'
    .param num a
    $N0 = a - 1
    a += 1
    a /= $N0
    a = ln a
    a /= 2
    .return (a)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
