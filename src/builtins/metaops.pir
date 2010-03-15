=head1 NAME

src/builtins/metaops.pir - meta-op operations

=head1 Functions

=over 2

=cut

.sub '!gen_not_metaop'
    .param string sym
    .local string opname, metaname
    $S0 = concat sym, '>'
    opname = concat '&infix:<', $S0
    metaname = concat '&infix:<!', $S0
    $P0 = get_global metaname
    unless null $P0 goto done
    $P1 = box opname
    .lex '$opname', $P1
    .const 'Sub' metasub = '!not_metaop'
    $P0 = newclosure metasub
    set_global metaname, $P0
  done:
.end

# XXX -- we might want this to be a Perl6MultiSub
.sub '!not_metaop' :anon :outer('!gen_not_metaop')
    .param pmc a
    .param pmc b
    $P0 = find_lex '$opname'
    $S0 = $P0
    $P0 = get_global $S0
    $P1 = $P0(a, b)
    .tailcall '&prefix:<!>'($P1)
.end

.sub '!gen_reverse_metaop'
    .param string sym
    .local string opname, metaname
    $S0 = concat sym, '>'
    opname = concat '&infix:<', $S0
    metaname = concat '&infix:<R', $S0
    $P0 = get_global metaname
    unless null $P0 goto done
    $P1 = box opname
    .lex '$opname', $P1
    .const 'Sub' metasub = '!reverse_metaop'
    $P0 = newclosure metasub
    set_global metaname, $P0
  done:
.end

# XXX -- we might want this to be a Perl6MultiSub
.sub '!reverse_metaop' :anon :outer('!gen_reverse_metaop')
    .param pmc a
    .param pmc b
    $P0 = find_lex '$opname'
    $S0 = $P0
    $P0 = get_global $S0
    $P1 = $P0(b, a)
    .return ($P1)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
