# $Id$

=head1 NAME

POST - A(nother) low-level opcode syntax tree.

=head1 DESCRIPTION

Compilers progress through various levels of
tree representations of compilation of a source
code program.  POST (Parrot Opcode Syntax Tree) is
a low-level tree which closely corresponds to the
semantics of PIR/PASM.

The base class of POST is Perl6::PAST::Node -- see C<lib/PAST.pir>

=head1 METHODS

=over 4

=cut

.namespace [ 'Perl6::POST' ]

.sub '__onload' :load
    .local pmc base
    $P0 = getclass 'Perl6::PAST::Node'
    base = subclass $P0, 'Perl6::POST::Node'
    addattribute base, '$.value'

    $P0 = subclass base, 'Perl6::POST::Val'
    addattribute $P0, '$.valtype'

    $P0 = subclass base, 'Perl6::POST::Var'
    addattribute $P0, '$.vartype'
    addattribute $P0, '$.isgen'

    $P0 = subclass base, 'Perl6::POST::Sub'
    addattribute $P0, '$.outer'

    $P0 = subclass base, 'Perl6::POST::Op'
    $P0 = subclass base, 'Perl6::POST::Ops'
    $P0 = subclass base, 'Perl6::POST::Label'
    $P0 = subclass base, 'Perl6::POST::Assign'

.end

.namespace [ 'Perl6::POST::Node' ]

.sub '__init' :method
    $P0 = new .String
    setattribute self, '$.name', $P0
    $P0 = new .String
    setattribute self, '$.value', $P0
    .return ()
.end

.sub 'name' :method
    .param pmc name            :optional
    .param int has_name        :opt_flag
    .return self.'attr'('$.name', name, has_name)
.end
  
=item C<Perl6::POST::Node::value()>

Set or return the invocant's value.  If no value has been
previously set for this node, then we generate a unique 
PMC register (uninitialized) and use that.

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

=item C<Perl6::POST::Label::value()>

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

.sub 'outer' :method
    .param pmc outer           :optional
    .param int has_outer       :opt_flag
    .return self.'attr'('$.outer', outer, has_outer)
.end

.sub 'pir' :method
    .local string name, outer
    name = self.'name'()
    outer = self.'outer'()
    if outer == '' goto outer_end
    outer = concat ":outer('", outer
    outer = concat outer, "')"
  outer_end:
    .local pmc code, iter, subcode
    code = new 'PGE::CodeString'
    code.'emit'("\n.sub '%0' %1", name, outer)
    subcode = new 'PGE::CodeString'
    iter = self.'child_iter'()
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $P1 = $P0.'pir'()
    $I0 = isa $P0, 'Perl6::POST::Sub'
    if $I0 goto concat_sub
    code .= $P1
    goto iter_loop
  concat_sub:
    subcode .= $P1
    goto iter_loop
  iter_end:
    .local string value
    value = self.'value'()
    code.'emit'("    .return (%0)\n.end\n", value)
    code = concat code, subcode
    .return (code)
.end

.sub '__dumplist' :method
    .return ('$.name $.outer $.value @.children')
.end

.namespace [ 'Perl6::POST::Val' ]

.sub 'value' :method
    .param string value        :optional
    .param int has_value       :opt_flag
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


.namespace [ 'Perl6::POST::Var' ]

.sub 'vartype' :method
    .param string vartype      :optional
    .param int has_vartype     :opt_flag
    .return self.'attr'('$.vartype', vartype, has_vartype)
.end

.sub 'isgen' :method
    .param string isgen        :optional
    .param int has_isgen       :opt_flag
    .return self.'attr'('$.isgen', isgen, has_isgen)
.end

.sub 'pir' :method
    ##   If we've already generated the pir for this variable, don't
    ##   do it a second time.
    $I0 = self.'isgen'()
    if $I0 == 0 goto gen_pir
    .return ('')

  gen_pir:
    .local pmc name, value, code
    name = self.'name'()
    value = self.'value'()
    code = new 'PGE::CodeString'
    code.'emit'("    %0 = find_global '%1'", value, name)
    self.'isgen'(1)
    .return (code)
.end


.sub 'assignpir' :method
    .param pmc x
    .local pmc name, value, xvalue, code
    name = self.'name'()
    value = self.'value'()
    xvalue = x.'value'()
    code = new 'PGE::CodeString'
    code.'emit'("    store_global '%0', %1", name, xvalue)
    code.'emit'("    %0 = %1", value, xvalue)
    self.'isgen'(1)
    .return (code)
.end


.namespace [ 'Perl6::POST::Assign' ]

.sub 'value' :method
    ##   return the value of our left hand side as our value
    $P0 = self[0]
    $P0 = $P0.'value'()
    .return ($P0)
.end


.sub 'pir' :method
    .local pmc rnode, rvalue
    rnode = self[1]
    rvalue = rnode.'value'()
    .local pmc lnode
    lnode = self[0]
    $P0 = lnode.'assignpir'(rnode)
    .return ($P0)
.end


