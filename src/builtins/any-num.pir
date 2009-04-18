## $Id$

=head1 NAME

src/builtins/any_num.pir -  C<Num>-like functions and methods for C<Any>

=head1 DESCRIPTION

This file implements the methods and functions of C<Any> that
are most closely associated with the C<Num> class or role.
We place them here instead of F<src/classes/Any.pir> to keep
the size of that file down and to emphasize their generic,
"built-in" nature.

=head2 Methods

=over 4

=cut

.namespace []
.sub 'onload' :anon :init :load
    $P0 = get_hll_namespace ['Any']
    '!EXPORT'('abs,int,log,polar,sqrt,truncate,unpolar', 'from'=>$P0)

    ##  pre-seed a random number generator
    $P0 = new 'Random'
    set_hll_global ['Any'], '$!random', $P0
    srand()
.end


=item abs()

=cut

.namespace ['Any']
.sub 'abs' :method :multi(_)
    $N0 = self
    $N1 = abs $N0
    .return ($N1)
.end

.namespace ['Any']
.sub 'int' :method :multi(_)
    .tailcall self.'truncate'()
.end

=item log

 our Num multi Num::log         ( Num $x: Num :$base )
 our Num multi Math::Basic::log ( Num $x, Num :$base )

Logarithm of base C<$base>, default Natural. Calling with C<$x == 0> is an
error.

=cut

.sub 'log' :method :multi(_)
    $N0 = self
    $N1 = ln $N0
    .return ($N1)
.end


=item polar

=cut

.namespace ['Any']
.sub 'polar' :method :multi(_)
    $N0 = self
    .tailcall 'list'($N0, 0)
.end


=item sqrt()

=cut

.namespace ['Any']
.sub 'sqrt' :method :multi(_)
    $N0 = self
    $N1 = sqrt $N0
    .return ($N1)
.end


=item srand()

=cut

.namespace []
.sub 'srand'
    .param num seed            :optional
    .param int has_seed        :opt_flag
    if has_seed goto have_seed
    seed = time
  have_seed:
    $P0 = get_hll_global ['Any'], '$!random'
    $I0 = seed
    $P0 = $I0
    .return ()
.end

.namespace ['Any']
.sub 'srand' :method
    $N0 = self
    $I0 = $N0
    $P0 = get_hll_global ['Any'], '$!random'
    $P0 = $I0
    .return ()
.end


=item truncate()

=item int

=cut

.namespace ['Any']
.sub 'truncate' :method :multi(_)
    $N0 = self
    if $N0 == 0 goto done
    if $N0 < 0 goto num_ceil
    floor $N0
    goto done
  num_ceil:
    ceil $N0
  done:
    $I0 = $N0
    .return ($I0)
.end


=item unpolar($angle)

=cut

=item roots

 our Array multi Num::roots ( Complex $z, Int $n )

Returns an Array consisting of the $n roots of a Complex number $z, where $n is
a positive integer. For any Complex number $z ( which includes real numbers and
integers as a subset ) there are a set of $n numbers W such that $w_k ** $n = $z,
or in set theory notation:

W = { $w_i : $w_i ** $n = $z and 0 <= i <= n-1 } .

These can be written in terms of the multiple-valued complex logarithm:

exp[1/$n*(Log($z)]

which is equal to

$w_k = exp[1/$n*(log($r)+i*($theta + 2*k*pi))] where k = 0,1,2,..., n-1

where ($r,$theta) = $z.polar . The angle $theta returned is always in the
interval -pi <= $theta <= pi .

=cut

.sub 'roots' :method
    .param int n
    .local num pi, r, theta
    .local pmc x, result, roots
    x        = self
    pi       = atan 1
    pi      *= 4
    roots    = new 'FixedPMCArray'
  if n > 0 goto positive
    roots    = 1                # single element array
    roots[0] = 'NaN'
    goto done
  positive:
    roots    = n                # fix array size to n
  if n > 1 goto general
    roots[0] = x
    goto done
  general:
    div $N0, 1, n
    $I0   = 0
    $I1   = isa x, 'Complex'
  unless $I1 goto real
    $N6   = x[0]
    $N7   = x[1]
    theta = atan $N7, $N6       # angle of polar representation
    $N6  *= $N6
    $N7  *= $N7
    $N8   = $N6 + $N7
    r     = sqrt $N8            # radius of polar representation
    $N1   = ln r
    goto loop
 real:
    $N4  = x
    $N4  = abs $N4              # if x < 0 we rotate by exp(i pi/n) later on
    $N1   = ln $N4              # ln(abs(x)) = ln(r)
 loop:
   if $I0 >= n goto done
    $P2    = new 'Complex'      # this can surely be optimized
    $N3    = $N0
    $N3   *= 2
    $N3   *= pi
    $N3   *= $I0
    $P2[1] = $N3                # 2*$I0*pi/n
    $N5    = $P2[1]
  unless $I1 goto rotate_negative_reals
    $N8    = $N0
    $N8   *= theta              # theta/n
    $N5   += $N8                # 2*$I0*pi/n + theta/n
    goto exponentiate
 rotate_negative_reals:         # we must rotate answer since we factored out (-1)^(1/n)
    if x > 0 goto exponentiate
    div $N4, pi, n
    $N5 += $N4                  # exp( i pi / n ) = (-1)^(1/n) (principle root)
  exponentiate:
    $N9        = $N0
    $N9       *= $N1            # 1/n*ln(r)
    $P2[0]     = $N9
    $P2[1]     = $N5
    $P2        = $P2.'exp'()    # exp(1/n*(ln(r)+i*(theta + 2*k*pi)))
    roots[$I0] = $P2
    inc $I0
    goto loop
  done:
    .return (roots)
.end

.sub 'unpolar' :method
    .param num angle
    .local num mag
    .local pmc result
    mag = self
    result = new 'Complex'
    $N0 = cos angle
    $N0 *= mag
    result[0] = $N0
    $N0 = sin angle
    $N0 *= mag
    result[1] = $N0
    .return (result)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
