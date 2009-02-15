## $Id$

=head1 NAME

src/classes/Junction.pir - Perl 6 Junction and related functions

=cut

# Constants for types of junctions.
.const int JUNCTION_TYPE_ANY  = 1
.const int JUNCTION_TYPE_ONE  = 2
.const int JUNCTION_TYPE_ALL  = 3
.const int JUNCTION_TYPE_NONE = 4

.namespace []
.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Junction', 'parent'=>'Perl6Object', 'attr'=>'@!eigenstates $!type')
.end

=head2 Methods

=over 4

=item perl()

Return perl representation.  (This should actually be autothreaded.)

=cut

.namespace ['Junction']
.sub 'perl' :method
    .local int type
    type = self.'!type'()

    .local string res
    if type == JUNCTION_TYPE_ANY goto any
    if type == JUNCTION_TYPE_ONE goto one
    if type == JUNCTION_TYPE_ALL goto all
    if type == JUNCTION_TYPE_NONE goto none
  any:
    res = 'any('
    goto type_done
  one:
    res = 'one('
    goto type_done
  all:
    res = 'all('
    goto type_done
  none:
    res = 'none('
  type_done:

    .local pmc it
    $P0 = self.'eigenstates'()
    it = iter $P0
    unless it goto states_done
    $P0 = shift it
    $S0 = $P0.'perl'()
    concat res, $S0
  states_loop:
    unless it goto states_done
    $P0 = shift it
    $S0 = $P0.'perl'()
    concat res, ', '
    concat res, $S0
    goto states_loop
  states_done:
    concat res, ')'
    .return (res)
.end


=item true()

Evaluate Junction as a boolean.

=cut

.namespace ['Junction']
.sub 'true' :method
    .local pmc eigenstates, it
    .local int type
    eigenstates = self.'eigenstates'()
    it = iter eigenstates
    type = self.'!type'()
    if type == JUNCTION_TYPE_NONE goto none
    if type == JUNCTION_TYPE_ALL goto all

  any_one:
    unless it goto false
    $P0 = shift it
    unless $P0 goto any_one
    if type == JUNCTION_TYPE_ANY goto true
    # fall through

  none:
    unless it goto true
    $P0 = shift it
    if $P0 goto false
    goto none

  all:
    unless it goto true
    $P0 = shift it
    if $P0 goto all
    # fall through

  false:
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)

  true:
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end

=item ACCEPTS

Smart-matching for junctions, short-circuiting.

=cut

.namespace ['Junction']
.sub 'ACCEPTS' :method
    .param pmc topic
    .local pmc eigenstates, it, state
    .local int type
    eigenstates = self.'eigenstates'()
    it = iter eigenstates
    type = self.'!type'()
    if type == JUNCTION_TYPE_NONE goto none
    if type == JUNCTION_TYPE_ALL goto all

  any_one:
    unless it goto false
    state = shift it
    $P0 = state.'ACCEPTS'(topic)
    unless $P0 goto any_one
    if type == JUNCTION_TYPE_ANY goto true
    # fall through

  none:
    unless it goto true
    state = shift it
    $P0 = state.'ACCEPTS'(topic)
    if $P0 goto false
    goto none

  all:
    unless it goto true
    state = shift it
    $P0 = state.'ACCEPTS'(topic)
    if $P0 goto all

  false:
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)

  true:
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end


=item !type()

Return the type of the Junction.

=item eigenstates()

Return the components of the Junction.

=cut

.namespace ['Junction']
.sub '!type' :method
    $P0 = getattribute self, '$!type'
    .return ($P0)
.end

.sub 'eigenstates' :method
    $P0 = getattribute self, '@!eigenstates'
    .return ($P0)
.end


=back

=head2 VTABLE functions

=cut

.namespace ['Junction']
.sub '' :method :vtable('get_bool')
    $I0 = self.'true'()
    .return ($I0)
.end


=head2 Helper functions

=cut

.namespace []
.sub '!MAKE_JUNCTION'
    .param pmc type
    .param pmc eigenstates

    .local pmc junc
    $P0 = get_hll_global 'Junction'
    junc = $P0.'new'()
    setattribute junc, '$!type', type

    # Make eigenstates unique if possible
    if type == JUNCTION_TYPE_ONE goto set_eigenstates
    $P0 = get_hll_global 'infix:==='
    eigenstates = eigenstates.'uniq'($P0)
  set_eigenstates:
    setattribute junc, '@!eigenstates', eigenstates
    .return (junc)
.end

=over

=item !DISPATCH_JUNCTION

Does a junctional dispatch. XXX Needs to support named args.

=cut

.sub '!DISPATCH_JUNCTION'
    .param pmc the_sub
    .param pmc args            :slurpy
    .param pmc name_args       :slurpy :named

    ##  lookup a sub by name if needed
    $I0 = isa the_sub, 'Sub'
    if $I0 goto have_sub
    $I0 = isa the_sub, 'MultiSub'
    if $I0 goto have_sub
    $S0 = the_sub
    the_sub = find_name $S0
  have_sub:

    ##  Look for the left-most junction.
    .local int argc, index, index_save
    argc = args
    index = 0
    index_save = -1
  left_loop:
    unless index < argc goto all_done
    .local pmc junc
    junc = args[index]
    $I0 = isa junc, 'Junction'
    if $I0 goto left_done
    inc index
    goto left_loop
  left_done:
    ##  If it's an all/none junction, we're good
    .local int type
    type = junc.'!type'()
    if type >= JUNCTION_TYPE_ALL goto have_index
    ##  one/any junction, so look through the remaining args for all/none
    index_save = index
    inc index
  all_loop:
    unless index < argc goto all_done
    junc = args[index]
    $I0 = isa junc, 'Junction'
    unless $I0 goto all_next
    type = junc.'!type'()
    if type >= JUNCTION_TYPE_ALL goto have_index
  all_next:
    inc index
    goto all_loop
  all_done:
    index = index_save
    junc = args[index]

    # If we don't have a junction now, need to check for anything in named.
    .local int found_junction
    found_junction = isa junc, 'Junction'
    unless found_junction goto check_named
    type = junc.'!type'()
  check_named:
    .local pmc name_iter, name_junc
    .local string cur_name, name_index
    name_iter = iter name_args
  name_loop:
    unless name_iter goto name_loop_end
    cur_name = shift name_iter
    name_junc = name_args[cur_name]
    $I0 = isa name_junc, 'Junction'
    unless $I0 goto name_loop
    $I0 = name_junc.'!type'()
    if $I0 >= JUNCTION_TYPE_ALL goto have_named_index
    if found_junction goto name_loop
  have_named_index:
    junc = name_junc
    type = $I0
    name_index = cur_name
  name_loop_end:

  have_index:
    .local pmc eigenstates, it, results
    eigenstates = junc.'eigenstates'()
    it = iter eigenstates
    results = 'list'()
  thread_loop:
    unless it goto thread_done
    $P0 = shift it
    unless null name_index goto thread_named
    args[index] = $P0
    goto do_threaded_call
  thread_named:
    name_args[name_index] = $P0
  do_threaded_call:
    $P0 = the_sub(args :flat, name_args :flat :named)
    push results, $P0
    goto thread_loop
  thread_done:
    .tailcall '!MAKE_JUNCTION'(type, results)
.end


=item !DISPATCH_JUNCTION_SINGLE

Wrapper for junction dispatcher in the single dispatch case, where we are
passed the sub that is being called along with a way to build tuples of the
parameters for the dispatcher.

=cut

.sub '!DISPATCH_JUNCTION_SINGLE'
    .param pmc sub
    .param pmc lexpad
    .param pmc signature

    # We build tuples of the args and pass them onto the main junction
    # dispatcher.
    .local pmc pos_args, name_args, it, param
    pos_args = new ['ResizablePMCArray']
    name_args = new ['Hash']
    $P0 = signature.'params'()
    it = iter $P0
  param_loop:
    unless it goto param_loop_end
    .local pmc param
    param = shift it
    .local string name
    .local pmc named, value
    name = param['name']
    named = param['named']
    value = lexpad[name]
    if null named goto pos_arg
    name_args[named] = value
    goto param_loop
  pos_arg:
    push pos_args, value
    goto param_loop
  param_loop_end:

    .tailcall '!DISPATCH_JUNCTION'(sub, pos_args :flat, name_args :flat :named)
.end


=item !DISPATCH_JUNCTION_MULTI

Wrapper for junction dispatcher in the multi dispatch case. Here we are handed
back as the thingy to call in place of a candidate, and PCC doesn't give us an
easy way to unshift another argument into the call, so we have it attached as
a property.

=cut

.sub '!DISPATCH_JUNCTION_MULTI'
    .param pmc pos_args  :slurpy
    .param pmc name_args :slurpy :named
    .local pmc pi, sub
    pi = new 'ParrotInterpreter'
    sub = pi['sub']
    sub = getprop 'sub', sub
    .tailcall '!DISPATCH_JUNCTION'(sub, pos_args :flat, name_args :flat :named)
.end

=back

=head2 Functions

=over 4

=item any(), infix:<|>()

=cut

.namespace []
.sub 'any'
    .param pmc args            :slurpy
    args.'!flatten'()
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ANY, args)
.end

.sub 'infix:|'
    .param pmc args            :slurpy
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ANY, args)
.end

=item one(), infix:<^>()

=cut

.namespace []
.sub 'one'
    .param pmc args            :slurpy
    args.'!flatten'()
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ONE, args)
.end

.sub 'infix:^'
    .param pmc args            :slurpy
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ONE, args)
.end


=item all(), infix:<&>()

=cut

.namespace []
.sub 'all'
    .param pmc args            :slurpy
    args.'!flatten'()
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ALL, args)
.end

.sub 'infix:&'
    .param pmc args            :slurpy
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_ALL, args)
.end


=item none()

=cut

.namespace []
.sub 'none'
    .param pmc args            :slurpy
    args.'!flatten'()
    .tailcall '!MAKE_JUNCTION'(JUNCTION_TYPE_NONE, args)
.end

=item infix:<~~>

=cut

.sub 'infix:~~' :multi('Junction', _)
    .param pmc topic
    .param pmc x
    .tailcall '!DISPATCH_JUNCTION'('infix:~~', topic, x)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
