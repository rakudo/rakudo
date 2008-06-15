## $Id$

=head1 NAME

src/builtins/globals.pir - initialize miscellaneous global variables

=cut

.namespace []

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'

    ##  set up %*ENV
    $P0 = get_hll_global 'Hash'
    p6meta.'register'('Env', 'parent'=>$P0, 'protoobject'=>$P0)
    $P0 = new 'Env'
    set_hll_global '%ENV', $P0

    ##  create $*IN, $*OUT, $*ERR filehandles
    .local pmc pio, perl6io, perl6ioclass
    perl6ioclass = get_hll_global "IO"
    pio = getstdin
    perl6io = perl6ioclass.'new'("PIO" => pio)
    set_hll_global "$IN", perl6io
    pio = getstdout
    perl6io = perl6ioclass.'new'("PIO" => pio)
    set_hll_global "$OUT", perl6io
    pio = getstderr
    perl6io = perl6ioclass.'new'("PIO" => pio)
    set_hll_global "$ERR", perl6io
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

