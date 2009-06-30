## $Id$

=head1 NAME

src/builtins/globals.pir - initialize miscellaneous global variables

=cut

.namespace []


.include 'interpinfo.pasm'
.include 'sysinfo.pasm'


.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'

    ##  set up %*ENV
    $P0 = get_hll_global 'Hash'
    p6meta.'register'('Env', 'parent'=>$P0, 'protoobject'=>$P0)
    .local pmc env
    env = root_new ['parrot';'Env']
    set_hll_global '%ENV', env

    ##  set up @*INC
    $S0 = env['PERL6LIB']
    $P0 = split ':', $S0
    push $P0, '.'
    $P0 = 'list'($P0)
    $P0 = $P0.'Array'()
    set_hll_global '@INC', $P0

    ##  set up %*INC
    $P0 = new ['Perl6Hash']
    set_hll_global '%INC', $P0

    ##  set up $*OS, $*OSVER $*EXECUTABLE_NAME
    .local string info
    info = sysinfo .SYSINFO_PARROT_OS
    $P0 = new ['Str']
    $P0 = info
    set_hll_global '$OS', $P0

    info = sysinfo .SYSINFO_PARROT_OS_VERSION
    $P0 = new ['Str']
    $P0 = info
    set_hll_global '$OSVER', $P0

    info = interpinfo .INTERPINFO_EXECUTABLE_FULLNAME
    $P0 = new ['Str']
    $P0 = info
    set_hll_global '$EXECUTABLE_NAME', $P0

    ## create basic $*CWD
    .local pmc os
    os = new ['OS']
    $S0 = os."cwd"()
    $P0 = box $S0
    set_hll_global '$CWD', $P0

    ##  create $*IN, $*OUT, $*ERR filehandles
    .local pmc pio, perl6io, perl6ioclass
    perl6ioclass = get_hll_global "IO"
    pio = getstdin
    pio.'encoding'('utf8')
    perl6io = perl6ioclass.'new'("PIO" => pio)
    set_hll_global "$IN", perl6io
    pio = getstdout
    pio.'encoding'('utf8')
    perl6io = perl6ioclass.'new'("PIO" => pio)
    set_hll_global "$OUT", perl6io
    pio = getstderr
    pio.'encoding'('utf8')
    perl6io = perl6ioclass.'new'("PIO" => pio)
    set_hll_global "$ERR", perl6io

    ##  set up %*VM
    load_bytecode 'config.pbc'
    .include 'iglobals.pasm'
    .local pmc vm, interp, config
    vm = new ['Perl6Hash']
    interp = getinterp
    config = interp[.IGLOBALS_CONFIG_HASH]
    vm['config'] = config
    set_hll_global "%VM", vm

    ## the default value for new ObjectRefs
    $P0 = 'undef'()
    set_hll_global '$!OBJECTREF', $P0
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

