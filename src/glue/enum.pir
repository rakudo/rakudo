=head1 NAME

src/glue/enum.pir -- internal handling of enums; kinda hacky

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

=item !setup_named_enum

=cut

.sub '!setup_named_enum'
    .param pmc name
    .param pmc values
    
    # For now, just install EnumMap under the main name.
    .local pmc full_ns, base_ns
    .local string shortname
    $P0 = get_hll_global ['Perl6';'Grammar'], 'parse_name'
    full_ns = $P0(name)
    base_ns = clone full_ns
    shortname = pop base_ns
    set_hll_global base_ns, shortname, values
    
    # Go over the keys/values and set them up.
    .local pmc it
    it = iter values
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    $S0 = $P0.'key'()
    $P1 = $P0.'value'()
    set_hll_global full_ns, $S0, $P1
    set_hll_global base_ns, $S0, $P1
    goto it_loop
  it_loop_end:
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
