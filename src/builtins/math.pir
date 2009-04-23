## $Id$

=head1 NAME

src/builtins/math.pir - Perl6 math functions

=head1 Math::Basic

=head2 Functions

=over 4

=cut

## TODO: figure out what to get working, in order to uncomment the following
## .namespace [ 'Math::Basic' ]


.sub 'roots' :multi(_, 'Integer')
    .param pmc x
    .param int n
    .local pmc result
     result = x.'roots'(n)
    .return (result)
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

Returns the sign of $x, i.e +1 for positive numbers (including Inf), zero for zero and -1 for negative numbers (including -Inf).

=cut

.sub 'sign'
    .param pmc a
    if a == 'Inf' goto unity
    if a == 'NaN' goto not_a_number
    $I0 = cmp_num a, 0
    .return ($I0)
  not_a_number:
    .return (a)
  unity:
    .return (1)
.end


=item exp

 our Num multi Num::exp         ( Num $exponent: Num :$base = Num::e )
 our Num multi Math::Basic::exp ( Num $exponent, Num :$base = Num::e )

Performs similar to C<$base ** $exponent>. C<$base> defaults to the
constant I<e>.

=cut

.sub 'exp' :multi(_)
    .param num a
    a = exp a
    .return (a)
.end


=item log10

 &log10 := &log.assuming:base(10);

Returns the base 10 logarithm of $x.

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

=item Inf / NaN

=cut

.sub 'Inf'
    $N0 = 'Inf'
    .return ($N0)
.end

.sub 'NaN'
    $N0 = 'NaN'
    .return ($N0)
.end


=item pi

 constant Num Num::pi = atan(1,1) * 4;
 constant Int Int::pi = 3;

=cut

.sub 'pi'
    .param pmc x               :slurpy
    ## 0-argument test, RT#56366
    unless x goto no_args
    die "too many arguments passed - 0 params expected"
  no_args:
    $N0 = atan 1
    $N0 *= 4
    .return ($N0)
.end


=item radcalc

=cut

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
    .local pmc    it

    if radix <= 1 goto err_range
    if radix > 36 goto err_range

    result       = 0.0
    fracdivisor = 1.0

    $P0 = split '', intpart
    it = iter $P0

  lp1: # Accumulate over decimal part
    unless it goto ex1
    $S0 = shift it
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
    unless it goto ex2
    $S0 = shift it
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

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
