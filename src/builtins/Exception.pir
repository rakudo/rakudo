=head1 TITLE

Array - Perl 6 Exception class

=head1 DESCRIPTION

A Perl 6 Exception object.

=head2 Methods

=over 4

=cut

.namespace [ 'Perl6Exception' ]

.sub '' :anon :init :load
    .local pmc p6meta, exceptionproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    exceptionproto = p6meta.'new_class'('Perl6Exception', 'parent'=>'Any parrot;Exception', 'attr'=>'$!exception', 'name'=>'Exception')
    p6meta.'register'('Exception', 'protoobject'=>exceptionproto)
.end

# IIUC we shouldn't need this... but Exception.new(:exception(...)) didn't work
.sub 'new' :method
    .param pmc ex
    .local pmc e, c
    c = self.'CREATE'('P6opaque')
    e = self.'bless'(c)
    setattribute e, '$!exception', ex
    .return (e)
.end

.sub exception :method
    .param pmc ex :optional
    .param int has_ex :opt_flag
    unless has_ex goto get_ex
    setattribute self, '$!exception', ex
    .return (ex)
  get_ex:
    ex = getattribute self, '$!exception'
    .return (ex)
.end


.sub 'resume' :method
    .local pmc ex, resume
    ex = getattribute self, '$!exception'
    resume = ex['resume']
    resume()
.end


.sub 'rethrow' :method
    .local pmc ex
    ex = getattribute self, '$!exception'
    rethrow ex
.end

.sub 'throw' :method
    .local pmc ex
    ex = getattribute self, '$!exception'
    throw ex
.end

.sub 'payload' :method
    .param pmc payload :optional
    .param int has_payload :opt_flag
    .local pmc ex
    ex = getattribute self, '$!exception'
    unless has_payload goto no_payload
    setattribute ex, 'payload', payload
    .return (payload)
  no_payload:
    payload = getattribute ex, 'payload'
    .return (payload)
.end

.sub 'handled' :method
    .param int handled :optional
    .param int has_handled :opt_flag
    .local pmc ex
    ex = getattribute self, '$!exception'
    unless has_handled goto no_handled
    ex['handled'] = handled
    .return (handled)
  no_handled:
    handled = ex['handled']
    .return (handled)
.end

.sub 'perl' :method
    .return ('undef')
.end


.sub '' :vtable('get_string') :method
    .tailcall self.'Str'()
.end

.sub 'Str' :method
    .local pmc exception
    exception = getattribute self, '$!exception'
    $S0 = exception['message']
    .return ($S0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
