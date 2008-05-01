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
    'die'("Type check failed")

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

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
