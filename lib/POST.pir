=head1 NAME

POST - A(nother) low-level opcode syntax tree.

=head1 DESCRIPTION

Compilers progress through various levels of
tree representations of compilation of a source
code program.  POST (Parrot Opcode Syntax Tree) is
a low-level tree which closely corresponds to the
semantics of PIR/PASM.

The base class of POST is Perl6::PAST::Node -- see C<lib/PAST.pir>

=cut

.namespace [ 'Perl6::POST' ]

.sub '__onload' :load
    .local pmc base
    $P0 = getclass 'Perl6::PAST::Node'
    base = subclass $P0, 'Perl6::POST::Node'
    addattribute base, '$.name'
    addattribute base, '$.value'

    $P0 = subclass base, 'Perl6::POST::Val'
    addattribute $P0, '$.valtype'

    $P0 = subclass base, 'Perl6::POST::Op'
    $P0 = subclass base, 'Perl6::POST::Ops'
    $P0 = subclass base, 'Perl6::POST::Label'
    $P0 = subclass base, 'Perl6::POST::Sub'

.end

.namespace [ 'Perl6::POST::Node' ]

.sub '__init' :method
    $P0 = new .String
    setattribute self, '$.name', $P0
    $P0 = new String
    setattribute self, '$.value', $P0
    .return ()
.end

.sub 'name' :method
    .param pmc name            :optional
    .param int has_name        :opt_flag
    .return self.'attr'('$.name', name, has_name)
.end
  
=item C<value()>

Set or return the invocant's value.  If no value has been
previously set for this node, then the default for POST::Node
is to use the value of its last child.  If it has no children,
then we generate a unique PMC register (uninitialized) and
use that.

=cut

.sub value :method
    .param pmc value           :optional
    .param int has_value       :opt_flag

    value = self.'attr'('$.value', value, has_value)
    if value > '' goto end
    $S0 = self.'unique'('$P')
    assign value, $S0
  end:
    .return (value)
.end


.sub '__dumplist' :method
    .return ('$.name $.value @.children')
.end


.namespace [ 'Perl6::POST::Ops' ]

.sub 'pir' :method
    .local pmc code, iter

    code = new 'PGE::CodeString'
    iter = self.'child_iter'()
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $P1 = $P0.'pir'()
    code .= $P1
    goto iter_loop
  iter_end:
    .return (code)
.end


.namespace [ 'Perl6::POST::Op' ]

.sub 'pir' :method
    .local pmc code

    .local pmc childvalues, iter
    childvalues = new .ResizablePMCArray
    iter = self.'child_iter'()
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $I0 = isa $P0, 'Perl6::POST::Node'
    if $I0 == 0 goto iter_loop_1
    $P0 = $P0.'value'()
  iter_loop_1:
    push childvalues, $P0
    goto iter_loop
  iter_end:

    code = new 'PGE::CodeString'
    .local string name
    name = self.'name'()
    $S0 = substr name, 0, 1
    if $S0 == "'" goto sub_call
    code.'emit'('    %n %,', childvalues :flat, 'n'=>name)
    .return (code)

  sub_call:
    $P0 = shift childvalues
    code.emit('    %r = %n(%,)', childvalues :flat, 'r'=>$P0, 'n'=>name)
    .return (code)
.end


.namespace [ 'Perl6::POST::Label' ]

=item C<value()>

Returns the value for this label.  If one hasn't already been
set, a new unique label is generated from the invocant's name
and that is returned.

=cut

.sub 'value' :method    
    .param pmc value           :optional
    .param int has_value       :opt_flag
    value = self.'attr'('$.value', value, has_value)
    if value > '' goto value_end
    .local pmc name
    name = self.'name'()
    $S0 = self.'unique'(name)
    assign value, $S0
  value_end:
    .return (value)
.end


.sub 'pir' :method
    .local string code
    .local string value
    value = self.'value'()
    code = '  '
    code .= value
    code .= ":\n"
    .return (code)
.end


.namespace [ 'Perl6::POST::Sub' ]

.sub 'pir' :method
    .local pmc code, iter
    code = new 'PGE::CodeString'
    code.'emit'(".sub 'anon' :anon")
    iter = self.'child_iter'()
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $P1 = $P0.'pir'()
    code .= $P1
    goto iter_loop
  iter_end:
    code.'emit'('.end')
    .return (code)
.end


.namespace [ 'Perl6::POST::Val' ]

.sub 'value' :method
    .param string value        :optional
    .param int has_value       :opt_flag
    .local pmc v, valtype
    .return self.'attr'('$.value', value, has_value)
.end

.sub 'valtype' :method
    .param string valtype      :optional
    .param int has_valtype     :opt_flag
    .return self.'attr'('$.valtype', valtype, has_valtype)
.end

.sub '__dumplist' :method
    .return ('$.name $.value $.valtype')
.end
