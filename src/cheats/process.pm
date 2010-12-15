# Really we should be able to write $PROCESS::IN := ...

package PROCESS {
    INIT {
        pir::getstdin__P().encoding('utf8');
        pir::getstdout__P().encoding('utf8');
        pir::getstderr__P().encoding('utf8');

        our $IN  = IO.new(:PIO(pir::getstdin__P));
        our $OUT = IO.new(:PIO(pir::getstdout__P));
        our $ERR = IO.new(:PIO(pir::getstderr__P));

        our $PERL = {
            name    => 'rakudo',
            version => Q:PIR { %r = box .RAKUDO_VERSION }
        };

        our $VM = {
            name    => 'parrot',
            config  =>
                Q:PIR {
                    .local pmc interp, config
                    .include 'iglobals.pasm'
                    load_bytecode 'config.pbc'
                    interp = getinterp
                    config = interp[.IGLOBALS_CONFIG_HASH]
                    %r = '&hash'(config :flat)
                }
        }

        our $TZ = ::DateTime-local-timezone.new;

        Q:PIR {
            ##  set up $*OS, $*OSVER $*EXECUTABLE_NAME
            .include 'sysinfo.pasm'
            .local string info
            info = sysinfo .SYSINFO_PARROT_OS_VERSION
            $P0 = new ['Str']
            $P0 = info
            set_hll_global ['PROCESS'], '$OSVER', $P0
            ##  do the OS last so that the PID workaround can use info
            info = sysinfo .SYSINFO_PARROT_OS
            $P0 = new ['Str']
            $P0 = info
            set_hll_global ['PROCESS'], '$OS', $P0
    
            # getpid() is still experimental:
            #  http://trac.parrot.org/parrot/ticket/1564 
            $P1 = getinterp
            $I0 = $P1.'getpid'()
            $P0 = box $I0
            set_hll_global ['PROCESS'], '$PID', $P0
        }
    }
}
