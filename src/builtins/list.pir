## $Id$

=head1 NAME

src/builtins/list.pir - Perl 6 list operators

=head1 Functions

=over 4

=item C<list(...)>

Build a List from its arguments.

=cut

.namespace

.sub 'list'
    .param pmc args            :slurpy
    .local pmc list, item
    list = new 'List'
  args_loop:
    unless args goto args_end
    item = shift args
    $I0 = defined item
    unless $I0 goto add_item
    # $I0 = isa item, 'Array'
    # if $I0 goto add_item
    $I0 = does item, 'array'
    unless $I0 goto add_item
    splice args, item, 0, 0
    goto args_loop
  add_item:
    push list, item
    goto args_loop
  args_end:
    .return (list)
.end


=item C<infix:,(...)>

Operator form for building a list from its arguments.

=cut

.sub 'infix:,'
    .param pmc args            :slurpy
    .return 'list'(args :flat)
.end


=item C<infix:X(...)>

The non-hyper cross operator.

=cut

.sub 'infix:X'
    .param pmc args            :slurpy
    .local pmc res
    res = new 'List'
    
    # Algorithm: we'll maintain a list of counters for each list, incrementing
    # the counter for the right-most list and, when it we reach its final
    # element, roll over the counter to the next list to the left as we go.
    .local pmc counters
    .local pmc list_elements
    .local int num_args
    counters = new 'FixedIntegerArray'
    list_elements = new 'FixedIntegerArray'
    num_args = elements args
    counters = num_args
    list_elements = num_args

    # Get element count for each list.
    .local int i
    .local pmc cur_list
    i = 0
elem_get_loop:
    if i >= num_args goto elem_get_loop_end
    cur_list = args[i]
    $I0 = elements cur_list
    list_elements[i] = $I0
    inc i
    goto elem_get_loop
elem_get_loop_end:

    # Now we'll start to produce them.
    .local int res_count
    res_count = 0
produce_next:
    
    # Start out by building list at current counters.
    .local pmc new_list
    new_list = new 'List'
    i = 0
cur_perm_loop:
    if i >= num_args goto cur_perm_loop_end
    $I0 = counters[i]
    $P0 = args[i]
    $P1 = $P0[$I0]
    new_list[i] = $P1
    inc i
    goto cur_perm_loop
cur_perm_loop_end:
    res[res_count] = new_list
    inc res_count
    
    # Now increment counters.
    i = num_args - 1
inc_counter_loop:
    $I0 = counters[i]
    $I1 = list_elements[i]
    inc $I0
    counters[i] = $I0

    # In simple case, we just increment this and we're done.
    if $I0 < $I1 goto inc_counter_loop_end
    
    # Otherwise we have to carry.
    counters[i] = 0

    # If we're on the first element, all done.
    if i == 0 goto all_done

    # Otherwise, loop.
    dec i
    goto inc_counter_loop
inc_counter_loop_end:
    goto produce_next

all_done:
    .return(res)
.end

## TODO: grep join map reduce reverse sort zip


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
