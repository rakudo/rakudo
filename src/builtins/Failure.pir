=head1 TITLE

Array - Perl 6 Exception class

=head1 DESCRIPTION

A Perl 6 Exception object.

=head2 Methods

=over 4

=cut

.namespace [ 'Failure' ]

.sub '' :anon :init :load
    .local pmc p6meta, failproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    failproto = p6meta.'new_class'('Failure', 'parent'=>'Any', 'attr'=>'$!ex', 'name'=>'Failure')
    p6meta.'register'('Failure', 'protoobject'=>failproto)
.end

# IIUC we shouldn't need this... but Exception.new(:exception(...)) didn't work
.sub 'new' :method
    .param pmc ex
    .local pmc e, c
    c = self.'CREATE'('P6opaque')
    e = self.'bless'(c)
    setattribute e, '$!ex', ex
    .return (e)
.end

.sub 'exception' :method
    .param pmc ex :optional
    .param int has_ex :opt_flag
    unless has_ex goto get_ex
    setattribute self, '$!ex', ex
    .return (ex)
  get_ex:
    ex = getattribute self, '$!ex'
    .return (ex)
.end

.sub 'perl' :method
    $P0 = self.'exception'()
    .tailcall $P0.'perl'()
.end

.sub 'maybefail' :method
    $P0 = self.'exception'()
    $I0 = $P0.'handled'()
    if $I0 goto okay
    $P0.'throw'()
  okay:
.end

.sub '' :vtable('get_bool') :method
    $I0 = self.'Bool'()
    .return ($I0)
.end

.sub 'Bool' :method
    $P0 = self.'exception'()
    $P0.'handled'(1)
    .return (0)
.end

.sub 'defined' :method
    $P0 = self.'exception'()
    $P0.'handled'(1)
    .return (0)
.end

.sub '' :vtable('defined') :method
    $I0 = self.'defined'()
    .return ($I0)
.end

.sub '' :vtable('get_string') :method
    $S0 = self.'Str'()
    .return ($S0)
.end

.sub 'Str' :method
    self.'maybefail'()
    .local pmc ex
    ex = getattribute self, '$!ex'
    $S0 = ex
    .return ($S0)
.end

.sub '' :vtable('get_number') :method
    $S0 = self.'Num'()
    .return ($S0)
.end

.sub 'Num' :method
    self.'maybefail'()
    $N0 = 0
    .return ($N0)
.end

.sub '' :vtable('get_integer') :method
    $S0 = self.'Int'()
    .return ($S0)
.end

.sub 'Int' :method
    self.'maybefail'()
    $I0 = 0
    .return ($I0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
