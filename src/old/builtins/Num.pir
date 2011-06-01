## $Id$

=head1 TITLE

Num - Perl 6 numbers

=head2 Methods

=over 4

=cut

.namespace [ 'Num' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta, numproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    numproto = p6meta.'new_class'('Num', 'parent'=>'parrot;Float Cool')

    # Map Parrot Float to Rakudo Num
    $P0 = getinterp
    $P1 = get_class ['Float']
    $P2 = get_class ['Num']
    $P0.'hll_map'($P1, $P2)
.end


=item succ and pred

Increment and Decrement Methods

=cut

.sub 'pred' :method
    $N0 = self
    dec $N0
    .return ($N0)
.end

.sub 'succ' :method
    $N0 = self
    inc $N0
    .return ($N0)
.end


=item WHICH()

Returns the identify value.

=cut

.sub 'WHICH' :method
    $N0 = self
    .return ($N0)
.end

=back

=head2 Operators

=over 4

=item &infix:<===>

Overridden for Num.

=cut

.namespace []
.sub '&infix:<===>' :multi(Float,Float)
    .param num a
    .param num b
    $I0 = iseq a, b
    .tailcall '&prefix:<?>'($I0)
.end

=back

=head2 Private methods

=over 4

=item radcalc

=cut

.namespace []

.sub '&radcalc'
    #this code was rewritten & optimized to be as dumb as possible. --lue
    .param int    radix #to all mathematicians, I know fractional radixes are possible. We programmers are taking baby steps :)
    .param string number
    .param num    base         :optional #as in exponents, not as in radixes
    .param int    has_base     :opt_flag
    .param num    exponent     :optional
    .param int    has_exponent :opt_flag
    .local int    iresult, fresult, fdivide
    .local num    result

    if radix <= 1 goto ERANGE
    if radix > 36 goto ERANGE #at least until we know how to represent bases > 36...
#magcheck:
    if has_base goto magcheck2 #if you hasn't a base, go on
        if has_exponent goto EEXPNOBASE #if you have an exponent but no base, ERROR!
            base = 1.0
            exponent = 1.0 #if you have no exponent and no base, then allow dumb coding to not affect the results.
            goto magcheckdone

magcheck2:
    if has_exponent goto magcheckdone #base+exponent? Continue
    goto EBASENOEXP #wait, you has a base, but no exponent? No way!
magcheckdone:
    $S99 = substr number, 0, 1
    if $S99 == "0" goto checkradix #maybe they entered :8<0x3F> ?
    goto allchecksdone

    #below is the nightmare of checking for radix conversion
checkradix:
    $S99 = substr number, 1, 1 #will they convert?
    $S99 = upcase $S99
    $I10 = index "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", $S99
    if $I10 >= radix goto changeradix #is the character beyond the range of the specified radix
    goto allchecksdone #false alarm
changeradix:
    if $S99 == "B" goto changeto2  #is it a binary number?
    if $S99 == "O" goto changeto8  #octal?
    if $S99 == "X" goto changeto16 #hex?
    if $S99 == "D" goto changeto10 #decimal?
    goto allchecksdone
changeto2:
    radix=2 #the actual radix change
    number = substr number, 2 #this is to remove what changed the radix (0x, 0b, etc.)
    goto allchecksdone #and, relax! The checks be over
changeto8:
    radix=8
    number = substr number, 2
    goto allchecksdone
changeto10:
    radix=10
    number = substr number, 2
    goto allchecksdone
changeto16:
    radix=16
    number = substr number, 2
    #above is the nightmare of checking for radix conversion

allchecksdone:
    iresult     = 0
    fresult     = 0
    fdivide     = 1
    $I99        = 0 #I don't want to hear about the Iter object. We DON'T NEED PMCs for a radix conversion.

iloop:
    $S0 = substr number, $I99, 1
    $S0 = upcase $S0 #I prefer uppercase when it comes to numbers in other bases, so that's how it's going to be.
    if $S0 == "_" goto iskip #if you seperate words, ex. DEAD_BEEF
    if $S0 == "." goto fbefore #fraction time!
    if $S0 == "" goto finish #all done
    $I0 = index "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", $S0
    if $I0 == -1 goto EINVALIDCHAR #say, you tried to pass off the number '3Z¿A5'. Jerk.
    if $I0 >= radix goto EINVALIDCHAR #if you try passing '2' with a radix of 2
    iresult *= radix
    iresult += $I0
iskip:
    $I99 += 1
    goto iloop #aaaand-a 1, 2, 3, 4...

fbefore:
    $I99 += 1 #we wouldn't want to read the . again, now do we?

floop:
    $S0 = substr number, $I99, 1 #no, we DO NOT reset the $I99 loop var.
    $S0 = upcase $S0
    if $S0 == "_" goto fskip #sepearation of words, again (could also be dwords, qwords...)
    if $S0 == "." goto EINVALIDCHAR #The IMU has yet to say you can write fractions of a fraction as 3.6.8 :)
    if $S0 == "" goto finish #all done!
    $I0 = index "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", $S0
    if $I0 == -1 goto EINVALIDCHAR #we do not know if 3@8 is a valid number. Especially considering we don't allow a radix that would have @ as a digit.
    if $I0 >= radix goto EINVALIDCHAR #remember kids, octals can't take the number 8
    fresult *= radix
    fresult += $I0
    fdivide *= radix #each place value of the fraction increases the denominator. If this is confusing, try converting any old decimal (say, .141592) into a fraction BY HAND.
fskip:
    $I99 += 1
    goto floop #and repeat

finish:
    $N0 = fresult / fdivide #for the fractional part
    result = iresult + $N0
    $N1 = pow base, exponent #for the magnitude. If no magnitude, then it's 1 pow 1
    result = result * $N1 #if no magnitude, then it's result * 1. Pefect :)

    .return (result)

#errors
ERANGE:
    die "The radix is out of range (2..36 only)"
EINVALIDCHAR:
    $S0 = concat "Invalid character (", $S0
    $S0 = concat $S0, ")! Please try again :) "
    die $S0
EBASENOEXP:
    die "You gave us a base for the magnitude, but you forgot the exponent."
EEXPNOBASE:
    die "You gave us an exponent for the magnitude, but you forgot the base."
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
