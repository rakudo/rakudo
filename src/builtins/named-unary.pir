## $Id: builtins.pir 12709 2006-05-17 01:42:08Z pmichaud $

=head1 NAME

src/builtins/named-unary.pir - Perl6 named unary builtins

=head1 Functions

=over 4

=cut

.namespace [ '' ]


.sub 'defined'
    .param pmc x
    $I0 = defined x
    .return ($I0)
.end


## string operators


.sub 'lc'
    .param string a
    downcase a
    .return (a)
.end


.sub 'uc'
    .param string a
    upcase a
    .return (a)
.end


.sub 'ucfirst'
    .param string a
    titlecase a
    .return (a)
.end


## TODO: lcfirst, capitalize, index, pack, pos, quotemeta, rindex, split
## sprintf, substr, unpack, vec, words


## math operators
## TODO: someday, these may exist in Math::Basic and / or Num namespaces
## until this is clarified, they exist in the root namespace


.sub 'abs'
    .param pmc a
    $P0 = abs a
    .return ($P0)
.end


.sub 'ceiling'
    .param num a
    ceil a
    .return (a)
.end


.sub 'floor'
    .param num a
    floor a
    .return (a)
.end


## TODO: rand srand


.sub 'round'
    .param num a
    a += 0.5
    $N0 = floor a
    .return (a)
.end


.sub 'sign'
    .param pmc a
    $I0 = cmp_num a, 0
    .return ($I0)
.end


.sub 'sqrt'
    .param num a
    a = sqrt a
    .return (a)
.end


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


## trancendental math ops
## TODO: these only work with radians atm

.sub 'atan' :multi(_)
    .param num a
    $N0 = atan a
    .return ($N0)
.end


.sub 'atan' :multi(_, _)
    .param num a
    .param num b
    $N0 = atan a, b
    .return (N0)
.end


.sub 'cos'
    .param num a
    $N0 = cos a
    .return ($N0)
.end


.sub 'sin'
    .param num a
    $N0 = sin a
    .return ($N0)
.end


.sub 'tan'
    .param num a
    $N0 = tan a
    .return ($N0)
.end


## TODO: e and pi should be constants, not subs
.sub 'e'
    $N0 = exp 1
.end


.sub 'pi'
    $N0 = atan 1
    $N0 *= 4
    .return ($N0)
.end


.sub 'exp'
    .param num a
    a = exp a
    .return (a)
.end


.sub 'log'
    .param num a
    $N0 = ln a
    .return ($N0)
.end


.sub 'log10'
    .param num a
    $N0 = log10 a
    .return ($N0)
.end



.namespace [ 'Math'; 'Trig' ]



## XXX: conjectural as perl6 subs do not support adverbs yet
## once this is correct, it will likely become a generated function, as will:
## sin, cos, tan, asin, acos, atan, sec, cosec, cotan, asec, acosec,
## acotan, sinh, cosh, tanh, asinh, acosh, atanh, sech, cosech, cotanh,
## asech, acosech, acotanh
.sub 'sin'
    .param num a
    .param pmc adverbs :slurpy :named
    .local num converter
    converter = 1
    $I0 = exists adverbs['$base']
    unless $I0 goto doit
    $P0 = adverbs['base']
    unless $P0 goto doit
    $S0 = $P0
    downcase $S0
    $S1 = substr $S0, 0, 1
    $I0 = index 'rdg123456789', $S1
    if $I0 == -1 goto err_unrecognized_base
    unless $I0 goto doit
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
    $N0 = sin a
    .return ($N0)
  err_unrecognized_base:
    $S1 = "sin: unrecognized base '"
    $S1 .= $S0
    $S1 .= "'"
    .return 'die'($S1)
.end


=back

=cut


## vim: expandtab sw=4
