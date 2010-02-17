=head1 NAME

src/glue/enum.pir -- internal handling of enums

=head2 Subs

=over 4

=item !create_anon_enum(value_list)

Constructs a EnumMap, based upon the values list.

=cut

.sub '!create_anon_enum'
    .param pmc values

    # Put the values into list context, so case of a single valued enum works.
    values = values.'list'()

    # For now, we assume integer type, unless we have a first pair that says
    # otherwise.
    .local pmc cur_val
    cur_val = box 0

    # Iterate over values and make mapping.
    .local pmc enumhash, values_it, cur_item
    enumhash = root_new ['parrot';'Hash']
    values_it = iter values
  values_loop:
    unless values_it goto values_loop_end
    cur_item = shift values_it
    $I0 = isa cur_item, 'Pair'
    if $I0 goto pair

  nonpair:
    enumhash[cur_item] = cur_val
    cur_val = cur_val.'succ'()
    goto values_loop

  pair:
    cur_val = cur_item.'value'()
    $P0 = cur_item.'key'()
    enumhash[$P0] = cur_val
    cur_val = cur_val.'succ'()
    goto values_loop

  values_loop_end:
    .local pmc result
    $P0 = get_hll_global 'EnumMap'
    result = $P0.'new'(enumhash :flat :named)
    .return (result)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
