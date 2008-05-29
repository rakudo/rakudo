## $Id$

=head1 NAME

src/builtins/guts.pir - subs that are part of the internals, not for users

=head1 SUBS

=over 4

=item !TYPECHECKEDASSIGN

Checks that the value and the assignee are type-compatible and does the
assignment.

=cut

.sub '!TYPECHECKEDASSIGN'
    .param pmc assignee
    .param pmc value

    # Any type information?
    .local pmc props, type_info
    push_eh do_assign
    props = getattribute assignee, '%!properties'
    type_info = props['vartype']
    pop_eh
    if null type_info goto do_assign
    $I0 = type_info.ACCEPTS(value)
    if $I0 goto do_assign
    $I0 = value.'isa'('Failure')
    if $I0 goto do_assign_failure
    'die'("Type check failed")

  do_assign_failure:
    # If it's a class type, we want to assign it's proto-object.
    push_eh do_assign
    $I0 = isa type_info, 'Perl6Protoobject'
    unless $I0 goto do_assign
    value = type_info
    goto do_assign

  do_assign:
    eq_addr assignee, value, no_copy
    copy assignee, value
    push_eh no_copy
    setattribute assignee, '%!properties', props
    pop_eh
no_copy:
    .return(assignee)
.end


=item !TYPECHECKPARAM

Checks the type of a parameter.

=cut

.sub '!TYPECHECKPARAM'
    .param pmc type
    .param pmc value
    $P0 = getinterp
    $P0 = $P0['lexpad';1]
    if null $P0 goto no_match_to_copy
    $P0 = $P0['$/']
    .lex "$/", $P0
  no_match_to_copy:

    $I0 = type.ACCEPTS(value)
    if $I0 goto ok
    'die'('Parameter type check failed')
ok:
.end


=item !keyword_class(name)

Internal helper method to create a class.

=cut

.sub '!keyword_class'
    .param string name
    .local pmc class, resolve_list, methods, iter

    # Create class.
    class = newclass name

    # Set resolve list to include all methods of the class.
    methods = inspect class, 'methods'
    iter = new 'Iterator', methods
    resolve_list = new 'ResizableStringArray'
  resolve_loop:
    unless iter goto resolve_loop_end
    $P0 = shift iter
    push resolve_list, $P0
    goto resolve_loop
  resolve_loop_end:
    class.resolve_method(resolve_list)

    .return(class)
.end

=item !keyword_role(name)

Internal helper method to create a role.

=cut

.sub '!keyword_role'
    .param string name
    .local pmc info, role

    # Need to make sure it ends up attached to the right
    # namespace.
    info = new 'Hash'
    info['name'] = name
    $P0 = new 'ResizablePMCArray'
    $P0[0] = name
    info['namespace'] = $P0

    # Create role.
    role = new 'Role', info

    # Stash in namespace.
    $P0 = new 'ResizableStringArray'
    set_hll_global $P0, name, role

    .return(role)
.end

=item !keyword_grammar(name)

Internal helper method to create a grammar.

=cut

.sub '!keyword_grammar'
    .param string name
    .local pmc info, grammar

    # Need to make sure it ends up attached to the right
    # namespace.
    info = new 'Hash'
    info['name'] = name
    $P0 = new 'ResizablePMCArray'
    $P0[0] = name
    info['namespace'] = $P0

    # Create grammar class..
    grammar = new 'Class', info

    .return(grammar)
.end

=item !keyword_does(class, role_name)

Internal helper method to implement the functionality of the does keyword.

=cut

.sub '!keyword_does'
    .param pmc class
    .param string role_name
    .local pmc role
    role = get_hll_global role_name
    addrole class, role
.end

=item !keyword_has(class, attr_name)

Adds an attribute with the given name to the class.

=cut

.sub '!keyword_has'
    .param pmc class
    .param string attr_name
    addattribute class, attr_name
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
