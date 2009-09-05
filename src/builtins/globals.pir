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
    .local pmc env
    env = '!env_to_hash'()
    set_hll_global ['PROCESS'], '%ENV', env

    ##  set up $*OS, $*OSVER $*EXECUTABLE_NAME
    .local string info
    info = sysinfo .SYSINFO_PARROT_OS
    $P0 = new ['Str']
    $P0 = info
    set_hll_global ['PROCESS'], '$OS', $P0

    info = sysinfo .SYSINFO_PARROT_OS_VERSION
    $P0 = new ['Str']
    $P0 = info
    set_hll_global ['PROCESS'], '$OSVER', $P0

    info = interpinfo .INTERPINFO_EXECUTABLE_FULLNAME
    $P0 = new ['Str']
    $P0 = info
    set_hll_global ['PROCESS'], '$EXECUTABLE_NAME', $P0

    ## create basic $*CWD
    .local pmc os
    os = root_new ['parrot';'OS']
    $S0 = os."cwd"()
    $P0 = box $S0
    set_hll_global '$CWD', $P0

    ##  create $*IN, $*OUT, $*ERR filehandles
    .local pmc pio, perl6io, perl6ioclass
    perl6ioclass = get_hll_global "IO"
    pio = getstdin
    pio.'encoding'('utf8')
    perl6io = perl6ioclass.'new'("PIO" => pio)
    set_hll_global ['PROCESS'], "$IN", perl6io
    pio = getstdout
    pio.'encoding'('utf8')
    perl6io = perl6ioclass.'new'("PIO" => pio)
    set_hll_global ['PROCESS'], "$OUT", perl6io
    pio = getstderr
    pio.'encoding'('utf8')
    perl6io = perl6ioclass.'new'("PIO" => pio)
    set_hll_global ['PROCESS'], "$ERR", perl6io

    ##  set up %*VM
    load_bytecode 'config.pbc'
    .include 'iglobals.pasm'
    .local pmc vm, interp, config
    vm = new ['Perl6Hash']
    interp = getinterp
    config = interp[.IGLOBALS_CONFIG_HASH]
    config = new ['Perl6Scalar'], config
    vm['config'] = config
    set_hll_global ['PROCESS'], "%VM", vm

    ##  set up @*INC
    $S0 = env['PERL6LIB']
    $P0 = split ':', $S0
    config = interp[.IGLOBALS_CONFIG_HASH]
    $S0 = config['libdir']
    $S1 = config['versiondir']
    concat $S0, $S1
    concat $S0, '/languages/perl6/lib'
    unshift $P0, $S0
    $S0 = env['HOME']
    concat $S0, '/.perl6lib'
    unshift $P0, $S0
    push $P0, '.'
    $P0 = 'list'($P0)
    $P0 = $P0.'Array'()
    set_hll_global ['PROCESS'], '@INC', $P0

    ##  set up %*INC
    $P0 = new ['Perl6Hash']
    set_hll_global ['PROCESS'], '%INC', $P0

    ## the default value for new ObjectRefs
    $P0 = 'undef'()
    set_hll_global '$!OBJECTREF', $P0
.end


.namespace []
.sub '!find_contextual'
    .param string name

    # first search caller scopes
    $P0 = find_dynamic_lex name
    unless null $P0 goto done

    # next, strip twigil and search PROCESS package
    .local string pkgname
    pkgname = clone name
    substr pkgname, 1, 1, ''
    $P0 = get_hll_global ['PROCESS'], pkgname
    unless null $P0 goto done
    $P0 = get_global pkgname
    unless null $P0 goto done

    # if still not found, try %*ENV
    .local pmc env
    env = '!find_contextual'('%*ENV')
    .local string envname
    envname = clone name
    substr envname, 0, 2, ''
    $I0 = exists env[envname]
    unless $I0 goto fail
    $P0 = env[envname]
    unless null $P0 goto done
  fail:
    $P0 = '!FAIL'('Contextual ', name, ' not found')
  done:
    .return ($P0)
.end


.sub '!env_to_hash'
    .local pmc env, hash
    env = root_new ['parrot';'Env']
    hash = new ['Perl6Hash']
    $P0 = iter env
  env_loop:
    unless $P0 goto env_done
    $S0 = shift $P0
    $S1 = env[$S0]
    hash[$S0] = $S1
    goto env_loop
  env_done:
    .return (hash)
.end


.sub '!hash_to_env'
    .param pmc hash            :optional
    .param int has_hash        :opt_flag

    if has_hash goto have_hash
    hash = '!find_contextual'('%*ENV')
  have_hash:

    .local pmc env
    env = root_new ['parrot';'Env']
    $P0 = iter env
  env_loop:
    unless $P0 goto env_done
    $S0 = shift $P0
    $I0 = exists hash[$S0]
    if $I0 goto env_loop
    delete env[$S0]
    goto env_loop
  env_done:

    $P0 = iter hash
  hash_loop:
    unless $P0 goto hash_done
    $S0 = shift $P0
    $S1 = hash[$S0]
    env[$S0] = $S1
    goto hash_loop
  hash_done:
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

