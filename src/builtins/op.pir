## $Id$

=head1 NAME

src/builtins/op.pir - Perl6 builtin operators

=head1 Functions

=over 4

=cut

.namespace []

## autoincrement
.sub 'postfix:++' :multi(_)
    .param pmc a
    $P0 = clone a
    inc a
    .return ($P0)
.end

.sub 'postfix:--' :multi(_)
    .param pmc a
    $P0 = clone a
    dec a
    .return ($P0)
.end


.sub 'prefix:++' :multi(_)
    .param pmc a
    inc a
    .return (a)
.end


.sub 'prefix:--' :multi(_)
    .param pmc a
    dec a
    .return (a)
.end


## exponentiation
.sub 'infix:**' :multi(_,_)
    .param num base
    .param num exp
    $N0 = pow base, exp
    .return ($N0)
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
    .return 'prefix:!'(a)
.end


.sub 'prefix:+' :multi(_)
    .param num a
    .return (a)
.end


.sub 'prefix:+' :multi('Integer')
    .param int a
    .return (a)
.end


.sub 'prefix:-' :multi(_)
    .param num a
    $N0 = neg a
    .return ($N0)
.end


.sub 'prefix:-' :multi('Integer')
    .param int a
    $I0 = neg a
    .return ($I0)
.end


.sub 'prefix:~' :multi(_)
    .param string a
    .return (a)
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


## multiplicative
.sub 'infix:*' :multi(_,_)
    .param num a
    .param num b
    $N0 = a * b
    .return ($N0)
.end


.sub 'infix:/' :multi(_,_)
    .param num a
    .param num b
    $N0 = a / b
    .return ($N0)
.end


.sub 'infix:%' :multi(_,_)
    .param num a
    .param num b
    $N0 = mod a, b
    .return ($N0)
.end


.sub 'infix:x' :multi(_,_)
    .param string a
    .param int b
    $S0 = repeat a, b
    .return ($S0)
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
.sub 'infix:+' :multi(_,_)
    .param num a
    .param num b
    $N0 = a + b
    .return ($N0)
.end


.sub 'infix:-' :multi(_,_)
    .param num a
    .param num b
    $N0 = a - b
    .return ($N0)
.end


.sub 'infix:~' :multi(_,_)
    .param string a
    .param string b
    $S0 = concat a, b
    .return ($S0)
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
    .return 'prefix:?'(a)
.end


.sub 'not' :multi(_)
    .param pmc a
    .return 'prefix:!'(a)
.end


.sub 'infix:.?'
    .param pmc invocant
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # For now we won't worry about signature, just if a method exists.
    $I0 = can invocant, method_name
    if $I0 goto invoke
    $P0 = get_hll_global 'Failure'
    .return ($P0)

    # If we do have a method, call it.
  invoke:
    .return invocant.method_name(pos_args :flat, named_args :named :flat)
.end


.sub 'infix:.*'
    .param pmc invocant
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Return an empty list if no methods exist at all.
    $I0 = can invocant, method_name
    if $I0 goto invoke
    .return 'list'()

    # Now find all methods and call them - since we know there are methods,
    # we just pass on to infix:.+.
  invoke:
    .return 'infix:.+'(invocant, method_name, pos_args :flat, named_args :named :flat)
.end


.sub 'infix:.+'
    .param pmc invocant
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # We need to find all methods we could call with the right name.
    .local pmc p6meta, result_list, class, mro, it, cap_class, failure_class
    result_list = 'list'()
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    class = invocant.'HOW'()
    class = p6meta.get_parrotclass(class)
    mro = inspect class, 'all_parents'
    it = iter mro
    cap_class = get_hll_global 'Capture'
    failure_class = get_hll_global 'Failure'
  mro_loop:
    unless it goto mro_loop_end
    .local pmc cur_class, meths, cur_meth
    cur_class = shift it
    meths = inspect cur_class, 'methods'
    cur_meth = meths[method_name]
    if null cur_meth goto mro_loop

    # If we're here, found a method. Invoke it and add capture of the results
    # to the result list.
    .local pmc pos_res, named_res, cap
    (pos_res :slurpy, named_res :named :slurpy) = cur_meth(invocant, pos_args :flat, named_args :named :flat)
    cap = cap_class.'!create'(failure_class, pos_res :flat, named_res :flat :named)
    push result_list, cap
    goto mro_loop
  mro_loop_end:

    # Make sure we got some elements, or we have to die.
    $I0 = elements result_list
    if $I0 == 0 goto failure
    .return (result_list)
  failure:
    $S0 = "Could not invoke method '"
    concat $S0, method_name
    concat $S0, "' on invocant of type '"
    $S1 = invocant.WHAT()
    concat $S0, $S1
    concat $S0, "'"
    'die'($S0)
.end


.sub 'infix:does'
    .param pmc var
    .param pmc role
    .param pmc init_value      :optional
    .param int have_init_value :opt_flag

    # Get the class of the variable we're adding roles to.
    .local pmc p6meta, parrot_class
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    parrot_class = p6meta.get_parrotclass(var)

    # Derive a new class that does the role(s) specified.
    .local pmc derived
    derived = new 'Class'
    addparent derived, parrot_class
    $I0 = isa role, 'Role'
    if $I0 goto one_role
    $I0 = isa role, 'List'
    if $I0 goto many_roles
    'die'("'does' expcts a role or a list of roles")

  one_role:
    '!keyword_does'(derived, role)
    goto added_roles

  many_roles:
    .local pmc role_it, cur_role
    role_it = iter role
  roles_loop:
    unless role_it goto roles_loop_end
    cur_role = shift role_it
    '!keyword_does'(derived, cur_role)
    goto roles_loop
  roles_loop_end:
  added_roles:

    # We need to make a dummy instance of the class, to force it to internally
    # construct itself.
    $P0 = new derived

    # Re-bless the object into the subclass and return it.
    rebless_subclass var, derived
    .return (var)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
