# $Id$


.namespace [ 'Failure' ]

.sub '' :anon :init :load
    .local pmc p6meta, failureproto, exceptionproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    failureproto = p6meta.'new_class'('Failure', 'parent'=>'Undef Any', 'attr'=>'$!exception')
    p6meta.'register'('Undef', 'parent'=>failureproto, 'protoobject'=>failureproto)

    $P0 = box 1
    set_hll_global '$WARNINGS', $P0
.end

=head2 Methods

=cut

.sub 'ACCEPTS' :method
    .param pmc topic
    $I0 = defined topic
    if $I0 goto defined
    .return(1)
  defined:
    .return(0)
.end


.sub 'defined' :method
    $P0 = self.'!exception'()
    $P0['handled'] = 1
    $P1 = get_hll_global ['Bool'], 'False'
    .return ($P1)
.end


.sub 'handled' :method
    .local pmc exception
    exception = self.'!exception'()
    $I0 = exception['handled']
    .return ($I0)
.end


.sub 'perl' :method
    .return ('undef')
.end


.namespace []
.sub 'undef'
    .param pmc x               :slurpy
    ## 0-argument test, RT#56366
    ## but see also C<< term:sym<undef> >> in STD.pm
    unless x goto no_args
    die "Obsolete use of undef; in Perl 6 please use undefine instead"
  no_args:
    $P0 = new 'Failure'
    .return ($P0)
.end


=head2 Private methods

=cut

.namespace ['Failure']
.sub '!exception' :method
    .local pmc exception
    exception = getattribute self, '$!exception'
    if null exception goto make_exception
    $I0 = isa exception, 'Exception'
    if $I0 goto have_exception
  make_exception:
    exception = new 'Exception'
    exception['message'] = 'Use of uninitialized value'
    setattribute self, '$!exception', exception
  have_exception:
    .return (exception)
.end


.sub '!throw_unhandled' :method
    $I0 = self.'handled'()
    if $I0 goto done
    $P0 = get_hll_global '$WARNINGS'
    unless $P0 goto done
    $P0 = self.'!exception'()
    $S0 = $P0['message']
    $S0 = concat $S0, "\n"
    .local pmc err
    err = get_hll_global "$ERR"
    err.'print'($S0)
  done:
.end


=head2 Vtable functions

=cut

.namespace ['Failure']
.sub '' :vtable('get_integer') :method
    self.'!throw_unhandled'()
    .return (0)
.end

.sub '' :vtable('get_number') :method
    self.'!throw_unhandled'()
    .return (0.0)
.end

.sub '' :vtable('get_string') :method
    self.'!throw_unhandled'()
    .return ('')
.end

.sub '' :vtable('get_pmc_keyed') :method
    .param pmc key
    .return (self)
.end

.sub '' :vtable('get_pmc_keyed_int') :method
    .param int key
    .return (self)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
