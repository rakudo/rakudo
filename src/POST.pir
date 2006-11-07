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

.sub '__onload' :load :init
    .local pmc base
    $P0 = getclass 'Perl6::PAST::Node'
    base = subclass $P0, 'Perl6::POST::Node'
    addattribute base, '$.value'

    $P0 = subclass base, 'Perl6::POST::Val'
    addattribute $P0, '$.valtype'

    $P0 = subclass base, 'Perl6::POST::Var'
    addattribute $P0, '$.scope'
    addattribute $P0, '$.islvalue'

    $P0 = subclass base, 'Perl6::POST::Sub'
    addattribute $P0, '$.outer'
    addattribute $P0, '$.subtype'
    addattribute $P0, '%!varhash'
    addattribute $P0, '$!prologue'

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
    .param pmc block
    .local pmc code, iter

    code = new 'PGE::CodeString'
    iter = self.'child_iter'()
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $P1 = $P0.'pir'(block)
    code .= $P1
    goto iter_loop
  iter_end:
    .return (code)
.end


.namespace [ 'Perl6::POST::Op' ]

.sub 'pir' :method
    .param pmc block
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

    .local pmc code
    code = new 'PGE::CodeString'
    $P0 = self.'name'()
    # $I0 = isa $P0, 'Perl6::POST::Sub'
    # if $I0 goto get_sub_name
    $I0 = isa $P0, 'Perl6::POST::Node'
    if $I0 goto get_sub_value
    .local string name
    name = $P0
    $S0 = substr name, 0, 1
    if $S0 == "'" goto sub_call
    code.'emit'('    %n %,', childvalues :flat, 'n'=>name)
    .return (code)
  get_sub_name:
    name = $P0.'name'()
    name = concat "'", name
    name = concat name, "'"
    goto sub_call
  get_sub_value:
    name = $P0.'value'()
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
    .param pmc block
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

.sub 'subtype' :method
    .param pmc subtype         :optional
    .param int has_subtype     :opt_flag
    .return self.'attr'('$.subtype', subtype, has_subtype)
.end

.sub 'varhash' :method
    .param pmc varhash         :optional
    .param int has_varhash     :opt_flag
    .return self.'attr'('%!varhash', varhash, has_varhash)
.end

.sub 'prologue' :method
    .param pmc prologue         :optional
    .param int has_prologue     :opt_flag
    .return self.'attr'('$!prologue', prologue, has_prologue)
.end

.sub 'root_pir' :method
    ##   create a new CodeString for the subs
    $P0 = new 'PGE::CodeString'
    store_global 'Perl6::POST', '$!subpir', $P0

    ##   build the pir for this node and its children
    self.'pir'(self)

    ##   get the generated code and return it
    $P0 = find_global 'Perl6::POST', '$!subpir'
    .return ($P0)
.end


.sub 'pir' :method
    .param pmc block
    .local string subtype
    subtype = self.'subtype'()
    if subtype != 'regex' goto standard_sub
    .return self.'pir_regex'()

  standard_sub:
    ##   create a new (empty) variable hash and prologue for this sub
    .local pmc varhash, prologue
    varhash = new .Hash
    self.'varhash'(varhash)
    prologue = new 'PGE::CodeString'
    self.'prologue'(prologue)

    .local string name, outerattr
    .local pmc outer
    name = self.'name'()
    outer = self.'outer'()
    outerattr = ''
    $I0 = defined outer
    if $I0 == 0 goto with_outerattr
    outerattr = outer.'name'()
    outerattr = concat ":outer('", outerattr
    outerattr = concat outerattr, "')"
  with_outerattr:
    .local pmc code, iter, subcode
    ## build the code for this sub
    prologue.'emit'("\n.sub '%0' %1", name, outerattr)
    ## add the $/ lexical
    $P0 = new 'Perl6::POST::Var'
    $P0.'init'('name'=>'$/', 'scope'=>'lexical')
    subcode = $P0.'pir'(self)
    iter = self.'child_iter'()
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $P1 = $P0.'pir'(self)
    subcode .= $P1
    goto iter_loop
  iter_end:
    .local string value
    prologue = self.'prologue'()
    subcode = concat prologue, subcode
    value = self.'value'()
    subcode.'emit'("    .return (%0)\n.end\n", value)
    ##   add the code to the current set of subs
    $P0 = find_global 'Perl6::POST', '$!subpir'
    subcode .= $P0
    store_global 'Perl6::POST', '$!subpir', subcode

    ##  generate the pir to locate this sub and return it
    code = new 'PGE::CodeString'
    code.'emit'("    %0 = find_name '%1'", value, name)
    .return (code)
.end

.sub 'pir_regex' :method
    .local pmc p6regex, regexast, regexpir
    .local string name, value
    name = self.'name'()
    value = self.'value'()
    p6regex = compreg 'PGE::P6Regex'
    regexast = self[0]
    regexpir = p6regex(regexast, 'name'=>name, 'grammar'=>'', 'target'=>'PIR')

    $P0 = find_global 'Perl6::POST', '$!subpir'
    regexpir .= $P0
    store_global 'Perl6::POST', '$!subpir', regexpir

    .local pmc code
    code = new 'PGE::CodeString'
    code.'emit'("    %0 = find_name '%1'", value, name)
    .return (code)
.end

.sub '__dumplist' :method
    .return ('$.name $.subtype $.outer $.value @.children')
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

.sub 'scope' :method
    .param string scope        :optional
    .param int has_scope       :opt_flag
    .return self.'attr'('$.scope', scope, has_scope)
.end

.sub 'islvalue' :method
    .param int islvalue        :optional
    .param int has_islvalue    :opt_flag
    .return self.'attr'('$.islvalue', islvalue, has_islvalue)
.end

.sub 'paramname' :method
    .local string name
    name = self.'name'()
    name = clone name
    $I0 = 0
  scalar_loop:
    $I0 = index name, '$', $I0
    if $I0 < 0 goto scalar_end
    substr name, $I0, 1, 'dollar_'
    goto scalar_loop
  scalar_end:
    .return (name)
.end

.sub 'pir' :method
    .param pmc block
    ##   if we already generated the code for this
    ##   variable, we generate nothing here.
    .local string name, scope, value
    .local pmc code
    .local int islvalue
    name = self.'name'()
    scope = self.'scope'()
    value = self.'value'()
    islvalue = self.'islvalue'()
    code = new 'PGE::CodeString'
    if scope == 'lexical' goto generate_lexical
    if scope == 'package' goto generate_package
    if scope == 'parameter' goto generate_parameter
    goto generate_find
  generate_parameter:
    .local pmc varhash
    varhash = block.'varhash'()
    $I0 = exists varhash[name]
    if $I0 goto generate_find
    .local pmc prologue
    .local string pname
    pname = self.'paramname'()
    prologue = block.'prologue'()
    prologue.'emit'("    .param pmc %0", pname)
    code.'emit'("    .lex '%0', %1", name, value)
    code.'emit'("    %0 = %1", value, pname)
    varhash[name] = self
    goto end
  generate_lexical:
    .local pmc varhash
    varhash = block.'varhash'()
    $I0 = exists varhash[name]
    if $I0 goto generate_find
    ##    This is the first time to see a lexical, generate its .lex
    code.'emit'("    .lex '%0', %1", name, value)
    varhash[name] = self
    goto end
  generate_package:
    if islvalue goto end
    code.'emit'("    %0 = find_global '%1'", value, name)
    goto end
  generate_find:
    if islvalue goto end
    code.'emit'("    %0 = find_name '%1'", value, name)
  end:
    .return (code)
.end


.sub 'assignpir' :method
    .param pmc block
    .param pmc x
    .local pmc name, value, scope, xvalue, code, varhash
    name = self.'name'()
    value = self.'value'()
    scope = self.'scope'()
    xvalue = x.'value'()
    code = new 'PGE::CodeString'
    varhash = block.'varhash'()
    $I0 = exists varhash[name]
    if $I0 goto with_varhash_name
    varhash[name] = self
  with_varhash_name:
    if scope == 'outerpackage' goto store_package
    if scope == 'package' goto store_package
  store_lexical:
    code.'emit'("    store_lex '%0', %1", name, xvalue)
    .return (code)
  store_package:
    code.'emit'("    store_global '%0', %1", name, xvalue)
    .return (code)
.end
    

.sub '__dumplist' :method
    .return ('$.name $.scope $.value')
.end

.namespace [ 'Perl6::POST::Assign' ]

.sub 'value' :method
    ##   return the value of our left hand side as our value
    $P0 = self[0]
    $P0 = $P0.'value'()
    .return ($P0)
.end


.sub 'pir' :method
    .param pmc block
    .local pmc rnode, rvalue
    rnode = self[1]
    rvalue = rnode.'value'()
    .local pmc lnode
    lnode = self[0]
    $P0 = lnode.'assignpir'(block, rnode)
    .return ($P0)
.end


