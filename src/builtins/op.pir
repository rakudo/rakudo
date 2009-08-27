## $Id$

=head1 NAME

src/builtins/op.pir - Perl 6 builtin operators

=head1 Functions

=over 4

=cut

.namespace []

## This is used by integer computations, to upgrade the answer and return a
## Num if we overflow. We may want to return something like a BigInt in the
## future, but we don't have that yet and this gives something closer to the
## correct semantics than not upgrading an Int at all.
.sub '!upgrade_to_num_if_needed'
    .param num test
    if test > 2147483647.0 goto upgrade
    if test < -2147483648.0 goto upgrade
    $I0 = test
    .return ($I0)
  upgrade:
    .return (test)
.end


## autoincrement
.sub 'prefix:++' :multi(_) :subid('!prefix:++')
    .param pmc a
    $I0 = defined a
    unless $I0 goto inc_undef
    $P1 = a.'succ'()
    .tailcall 'infix:='(a, $P1)
  inc_undef:
    .tailcall 'infix:='(a, 1)
.end

.sub 'postfix:++' :multi(_) :subid('!postfix:++')
    .param pmc a
    $P0 = a.'clone'()
    .const 'Sub' $P1 = '!prefix:++'
    $P1(a)
    .return ($P0)
.end

.sub 'prefix:--' :multi(_) :subid('!prefix:--')
    .param pmc a
    $I0 = defined a
    unless $I0 goto dec_undef
    $P1 = a.'pred'()
    .tailcall 'infix:='(a, $P1)
  dec_undef:
    .tailcall 'infix:='(a, -1)
.end

.sub 'postfix:--' :multi(_)
    .param pmc a
    $P0 = a.'clone'()
    .const 'Sub' $P1 = '!prefix:--'
    $P1(a)
    .return ($P0)
.end

.sub 'prefix:++' :multi(Integer) :subid('!prefix:++Int')
    .param pmc a
    unless a < 2147483647 goto fallback
    $P0 = getprop 'readonly', a
    unless null $P0 goto fallback
    $P0 = getprop 'type', a
    if null $P0 goto fast_inc
    $P1 = get_hll_global 'Int'
    $I0 = issame $P0, $P1
    unless $I0 goto fallback
  fast_inc:
    inc a
    .return (a)
  fallback:
    .const 'Sub' fb = '!prefix:++'
    .tailcall fb(a)
.end

.sub 'postfix:++' :multi(Integer)
    .param pmc a
    $P0 = deobjectref a
    $P0 = clone $P0
    .const 'Sub' $P1 = '!prefix:++Int'
    $P1(a)
    .return ($P0)
.end


## symbolic unary
.sub 'prefix:!' :multi(_)
    .param pmc a
    if a goto a_true
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
  a_true:
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
.end


.sub 'prefix:^?' :multi(_)
    .param pmc a
    .tailcall 'prefix:!'(a)
.end


.sub 'prefix:+' :multi(_)
    .param num a
    .return (a)
.end


.sub 'prefix:+' :multi('Integer')
    .param num a
    .tailcall '!upgrade_to_num_if_needed'(a)
.end


.sub 'prefix:-' :multi(_)
    .param num a
    $N0 = neg a
    .return ($N0)
.end


.sub 'prefix:-' :multi('Integer')
    .param num a
    $N0 = neg a
    .tailcall '!upgrade_to_num_if_needed'($N0)
.end


.sub 'prefix:~' :multi(_)
    .param string a
    $P0 = new ['Str']
    $P0 = a
    .return ($P0)
.end


.sub 'prefix:?' :multi(_)
    .param pmc a
    if a goto a_true
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
  a_true:
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end


## TODO: prefix:= prefix:* prefix:** prefix:~^ prefix:+^


.sub 'infix:%' :multi(_,_)
    .param num a
    .param num b
    $N0 = mod a, b
    .return ($N0)
.end


.sub 'infix:%' :multi(Integer,Integer)
    .param num a
    .param num b
    $N0 = mod a, b
    .tailcall '!upgrade_to_num_if_needed'($N0)
.end


.sub 'infix:xx' :multi(_,_)
    .param pmc a
    .param int n
    $P0 = 'list'()
  loop:
    unless n > 0 goto done
    push $P0, a
    dec n
    goto loop
  done:
    .return ($P0)
.end


.sub 'infix:+&' :multi(_,_)
    .param int a
    .param int b
    $I0 = band a, b
    .return ($I0)
.end


.sub 'infix:+<' :multi(_,_)
    .param int a
    .param int b
    $I0 = shl a, b
    .return ($I0)
.end


.sub 'infix:+>' :multi(_,_)
    .param int a
    .param int b
    $I0 = shr a, b
    .return ($I0)
.end


.sub 'infix:~&' :multi(_,_)
    .param string a
    .param string b
    $S0 = bands a, b
    .return ($S0)
.end


## TODO: infix:~< infix:~>


## additive

.sub 'infix:~' :multi(_,_)
    .param string a
    .param string b
    $S0 = concat a, b
    $P0 = new ['Str']
    assign $P0, $S0
    .return ($P0)
.end


.sub 'infix:+|'
    .param int a
    .param int b
    $I0 = bor a, b
    .return ($I0)
.end


.sub 'infix:+^'
    .param int a
    .param int b
    $I0 = bxor a, b
    .return ($I0)
.end


.sub 'infix:~|'
    .param string a
    .param string b
    $S0 = bors a, b
    .return ($S0)
.end


.sub 'infix:~^'
    .param string a
    .param string b
    $S0 = bxors a, b
    .return ($S0)
.end


.sub 'infix:?&'
    .param int a
    .param int b
    $I0 = band a, b
    $I0 = isne $I0, 0
    .return ($I0)
.end


.sub 'infix:?|'
    .param int a
    .param int b
    $I0 = bor a, b
    $I0 = isne $I0, 0
    .return ($I0)
.end


.sub 'infix:?^'
    .param int a
    .param int b
    $I0 = bxor a, b
    $I0 = isne $I0, 0
    .return ($I0)
.end


.sub 'true' :multi(_)
    .param pmc a
    .tailcall 'prefix:?'(a)
.end


.sub 'not' :multi(_)
    .param pmc a
    .tailcall 'prefix:!'(a)
.end


.sub 'infix:does'
    .param pmc var
    .param pmc role
    .param pmc init_value      :optional
    .param int have_init_value :opt_flag

    # Get the class of the variable we're adding roles to.
    .local pmc p6meta, parrot_class
    var.'!rebox'()
    parrot_class = class var

    # Derive a new class that does the role(s) specified.
    .local pmc derived
    derived = root_new ['parrot';'Class']
    addparent derived, parrot_class
    $I0 = isa role, ['Perl6Role']
    if $I0 goto one_role_select
    #$P0 = get_root_namespace ['parrot';'Role']
    #$P0 = get_class $P0
    $I0 = isa role, 'P6role'
    if $I0 goto one_role
    $I0 = isa role, ['List']
    if $I0 goto many_roles
  error:
    'die'("'does' expects a role or a list of roles")

  one_role_select:
    role = role.'!select'()
  one_role:
    addrole derived, role
    '!compose_role_attributes'(derived, role)
    goto added_roles

  many_roles:
    .local pmc role_it, cur_role
    role_it = iter role
  roles_loop:
    unless role_it goto roles_loop_end
    cur_role = shift role_it
    $I0 = isa cur_role, 'Role'
    if $I0 goto have_parrot_role
    $I0 = isa cur_role, 'Perl6Role'
    unless $I0 goto error
    cur_role = cur_role.'!select'()
  have_parrot_role:
    addrole derived, cur_role
    '!compose_role_attributes'(derived, cur_role)
    goto roles_loop
  roles_loop_end:
  added_roles:

    # Instantiate the class to make it form itself.
    $P0 = new derived

    # Create a new meta-class, but associate with existing proto-object.
    .local pmc p6meta, old_proto, new_proto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    new_proto = p6meta.'register'(derived)
    $P0 = new_proto.'HOW'()
    old_proto = var.'WHAT'()
    setattribute $P0, 'protoobject', old_proto

    # Re-bless the object into the subclass.
    rebless_subclass var, derived

    # We need to set any initial attribute values up.
    .lex '$CLASS', new_proto
    $P0 = find_method new_proto, 'BUILD'
    $P0(var)

    # If we were given something to initialize with, do so.
    unless have_init_value goto no_init
    .local pmc attrs
    .local string attr_name
    attrs = inspect role, "attributes"
    attrs = attrs.'keys'()
    $I0 = elements attrs
    if $I0 != 1 goto attr_error
    attr_name = attrs[0]
    attr_name = substr attr_name, 2 # lop off sigil and twigil
    $P0 = var.attr_name()
    'infix:='($P0, init_value)
  no_init:

    # We're done - return.
    .return (var)

attr_error:
    'die'("Can only supply an initialization value to a role with one attribute")
.end


.sub 'infix:but'
    .param pmc var
    .param pmc role
    .param pmc value      :optional
    .param int have_value :opt_flag

    # First off, is the role actually a role?
    $I0 = isa role, 'Perl6Role'
    if $I0 goto have_role
    $I0 = isa role, 'Role'
    if $I0 goto have_role

    # If not, it may be an enum. If we don't have a value, get the class of
    # the thing passed as a role and find out.
    if have_value goto error
    .local pmc maybe_enum
    maybe_enum = role.'WHAT'()
    $P0 = getprop '$!is_enum', maybe_enum
    if null $P0 goto error
    unless $P0 goto error
    value = role
    role = maybe_enum
    goto have_role
    unless null role goto have_role

    # Did anything go wrong?
  error:
    'die'("The but operator can only be used with a role or enum value on the right hand side")

    # Now we have a role, copy the value and call does on the copy.
  have_role:
    $I0 = isa var, 'ObjectRef'
    unless $I0 goto not_obj_ref
    var = deref var
  not_obj_ref:
    var = clone var
    if null value goto no_value
    'infix:does'(var, role, value)
    goto return
  no_value:
    'infix:does'(var, role)
  return:
    .return (var)
.end


=item !generate_meta_ops

Generates meta-ops for user defined operators.

=cut

.sub '!generate_meta_ops'
    .param string full_name
    .param string equiv

    # If op is already generated, defined, we're done.
    .local string name
    name = substr full_name, 6
    $S0 = concat 'infix:R', name
    $P0 = get_hll_global $S0
    unless null $P0 goto done

    # Generate all the names we'll need.
    .local string assignment, reverse, cross, reduce, hyper1, hyper2, hyper3, hyper4
    .local string hyper1_asc, hyper2_asc, hyper3_asc, hyper4_asc
    assignment = concat 'infix:', name
                 concat assignment, '='
    reverse    = concat 'infix:R', name
    cross      = concat 'infix:X', name
    reduce     = concat 'prefix:[', name
                 concat reduce, ']'
    hyper1_asc = concat 'infix:<<', name
                 concat hyper1_asc, '>>'
    hyper2_asc = concat 'infix:>>', name
                 concat hyper2_asc, '<<'
    hyper3_asc = concat 'infix:<<', name
                 concat hyper3_asc, '<<'
    hyper4_asc = concat 'infix:>>', name
                 concat hyper4_asc, '>>'
    hyper1     = concat unicode:"infix:\u00ab", name
                 concat hyper1, unicode:"\u00bb"
    hyper2     = concat unicode:"infix:\u00bb", name
                 concat hyper2, unicode:"\u00ab"
    hyper3     = concat unicode:"infix:\u00ab", name
                 concat hyper3, unicode:"\u00ab"
    hyper4     = concat unicode:"infix:\u00bb", name
                 concat hyper4, unicode:"\u00bb"

    # Add all of the tokens.
    .local pmc optable
    optable = get_hll_global ['Perl6';'Grammar'], '$optable'
    optable.'newtok'(assignment, 'equiv'=>'infix::=', 'lvalue'=>1)
    optable.'newtok'(reduce, 'equiv'=>'infix:=')
    optable.'newtok'(reverse, 'equiv'=>equiv)
    optable.'newtok'(cross, 'equiv'=>'infix:X')
    optable.'newtok'(hyper1, 'equiv'=>equiv)
    optable.'newtok'(hyper1_asc, 'equiv'=>equiv, 'subname'=>hyper1)
    optable.'newtok'(hyper2, 'equiv'=>equiv)
    optable.'newtok'(hyper2_asc, 'equiv'=>equiv, 'subname'=>hyper2)
    optable.'newtok'(hyper3, 'equiv'=>equiv)
    optable.'newtok'(hyper3_asc, 'equiv'=>equiv, 'subname'=>hyper3)
    optable.'newtok'(hyper4, 'equiv'=>equiv)
    optable.'newtok'(hyper4_asc, 'equiv'=>equiv, 'subname'=>hyper4)

    # Now generate the subs.
    $P0 = '!generate_meta_op_sub'('!generate_meta_op_helper_simple', '!ASSIGNMETAOP', name)
    set_hll_global assignment, $P0
    $P0 = '!generate_meta_op_sub'('!generate_meta_op_helper_reduce', name)
    set_hll_global reduce, $P0
    $P0 = '!generate_meta_op_sub'('!generate_meta_op_helper_reverse', full_name)
    set_hll_global reverse, $P0
    $P0 = '!FAIL'()
    $P0 = '!generate_meta_op_sub'('!generate_meta_op_helper_cross', name)
    set_hll_global cross, $P0
    $P0 = '!generate_meta_op_sub'('!generate_meta_op_helper_hyper', '!HYPEROP', name, 0, 0)
    set_hll_global hyper1, $P0
    $P0 = '!generate_meta_op_sub'('!generate_meta_op_helper_hyper', '!HYPEROP', name, 1, 1)
    set_hll_global hyper2, $P0
    $P0 = '!generate_meta_op_sub'('!generate_meta_op_helper_hyper', '!HYPEROP', name, 0, 1)
    set_hll_global hyper3, $P0
    $P0 = '!generate_meta_op_sub'('!generate_meta_op_helper_hyper', '!HYPEROP', name, 1, 0)
    set_hll_global hyper4, $P0
  done:
.end
.sub '!generate_meta_op_sub'
    .param string which_helper
    .param pmc delegate_to
    .param pmc args :slurpy
    .lex '$delegate_to', delegate_to
    .lex '@args', args
    $P0 = find_name which_helper
    $P0 = newclosure $P0
    .return ($P0)
.end
.sub '!generate_meta_op_helper_simple' :outer('!generate_meta_op_sub')
    .param pmc a
    .param pmc b
    $P0 = find_lex '$delegate_to'
    $S0 = $P0
    $P0 = find_name $S0
    $P1 = find_lex '@args'
    .tailcall $P0($P1 :flat, a, b)
.end
.sub '!generate_meta_op_helper_reverse' :outer('!generate_meta_op_sub')
    .param pmc a
    .param pmc b
    $P0 = find_lex '$delegate_to'
    $S0 = $P0
    $P0 = find_name $S0
    .tailcall $P0(b, a)
.end
.sub '!generate_meta_op_helper_reduce' :outer('!generate_meta_op_sub')
    .param pmc args :slurpy
    $P0 = find_lex '$delegate_to'
    .tailcall '!REDUCEMETAOP'($P0, 0, args :flat)
.end
.sub '!generate_meta_op_helper_cross' :outer('!generate_meta_op_sub')
    .param pmc args :slurpy
    $P0 = find_lex '$delegate_to'
    .tailcall '!CROSSMETAOP'($P0, 0, 0, args :flat)
.end
.sub '!generate_meta_op_helper_hyper' :outer('!generate_meta_op_sub')
    .param pmc a
    .param pmc b
    $P0 = find_lex '$delegate_to'
    $S0 = $P0
    $P0 = find_name $S0
    $P1 = find_lex '@args'
    $I1 = pop $P1
    $I0 = pop $P1
    .tailcall $P0($P1 :flat, a, b, $I0, $I1)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
