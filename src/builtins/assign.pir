## $Id$

=head1 NAME

src/builtins/inplace.pir - Inplace assignments

=head1 Functions

=over 4

=cut

.namespace

## assignment
## TODO: infix::= infix:::= infix:.=
##   -- these will likely be handled by compiler translation --Pm


.sub 'infix:~='
    .param pmc a
    .param pmc b
    concat a, b
    .return (a)
.end


.sub 'infix:+='
    .param pmc a
    .param pmc b
    a += b
    .return (a)
.end


.sub 'infix:-='
    .param pmc a
    .param pmc b
    a -= b
    .return (a)
.end


.sub 'infix:*='
    .param pmc a
    .param pmc b
    a *= b
    .return (a)
.end


.sub 'infix:/='
    .param pmc a
    .param pmc b
    a /= b
    .return (a)
.end


.sub 'infix:%='
    .param pmc a
    .param pmc b
    a %= b
    .return (a)
.end


.sub 'infix:x='
    .param pmc a
    .param pmc b
    repeat a, a, b
    .return (a)
.end


## TODO: infix:Y=
.sub 'infix:**='
    .param pmc a
    .param pmc b
    a = a ** b
    .return (a)
.end


## TODO: infix:xx= infix:||= infix:&&= infix://= infix:^^=


.sub 'infix:+<='
    .param pmc a
    .param pmc b
    a <<= b
    .return (a)
.end


.sub 'infix:+>='
    .param pmc a
    .param pmc b
    a >>= b
    .return (a)
.end


.sub 'infix:+&='
    .param pmc a
    .param pmc b
    band a, b
    .return (a)
.end


.sub 'infix:+|='
    .param pmc a
    .param pmc b
    bor a, b
    .return (a)
.end


.sub 'infix:+^='
    .param pmc a
    .param pmc b
    bxor a, b
    .return (a)
.end


.sub 'infix:~&='
    .param pmc a
    .param pmc b
    a = bands a, b
    .return (a)
.end


.sub 'infix:~|='
    .param pmc a
    .param pmc b
    bors a, b
    .return (a)
.end


.sub 'infix:~^='
    .param pmc a
    .param pmc b
    bxors a, b
    .return (a)
.end


.sub 'infix:?&='
    .param pmc a
    .param pmc b
    band a, b
    $I0 = istrue a
    a = $I0
    .return (a)
.end


.sub 'infix:?|='
    .param pmc a
    .param pmc b
    bor a, b
    $I0 = istrue a
    a = $I0
    .return (a)
.end


.sub 'infix:?^='
    .param pmc a
    .param pmc b
    bxor a, b
    $I0 = istrue a
    a = $I0
    .return (a)
.end


=back

=cut


## vim: expandtab sw=4

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
