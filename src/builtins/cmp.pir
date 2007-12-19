## $Id$

=head1 NAME

src/builtins/cmp.pir - Perl6 comparison builtins

=head1 Functions

=over 4

=cut

.namespace

.sub 'infix:=='
    .param num a
    .param num b
    $I0 = iseq a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:!='
    .param num a
    .param num b
    $I0 = isne a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:<'
    .param num a
    .param num b
    $I0 = islt a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:<='
    .param num a
    .param num b
    $I0 = isle a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:>'
    .param num a
    .param num b
    $I0 = isgt a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:>='
    .param num a
    .param num b
    $I0 = isge a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:<=>'
    .param pmc a
    .param pmc b
    $I0 = cmp_num a, b
    .return ($I0)
.end


.sub 'infix:eq'
    .param string a
    .param string b
    $I0 = iseq a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:ne'
    .param string a
    .param string b
    $I0 = isne a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:lt'
    .param string a
    .param string b
    $I0 = islt a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:le'
    .param string a
    .param string b
    $I0 = isle a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:gt'
    .param string a
    .param string b
    $I0 = isgt a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:ge'
    .param string a
    .param string b
    $I0 = isge a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:cmp'
    .param pmc a
    .param pmc b
    $I0 = cmp_str a, b
    .return ($I0)
.end


## TODO: infix:=:= infix:===


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
