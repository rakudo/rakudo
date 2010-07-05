# Really we should be able to write $PROCESS::IN := ...

package PROCESS {
    INIT {
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
    
            ##  Set up $*PID.  Parrot doesn't give us the PID for now.
            ##  idea: http://irclog.perlgeek.de/parrot/2010-04-19#i_2242900
            ##  Well, this file *is* in the cheats directory... :-)
            .local pmc library
            .local string getpid_func
            null library
            getpid_func = 'getpid'
            if info != 'MSWin32' goto setup_io_non_MSWin32
            ##  Do it differently on Windows
            library = loadlib 'kernel32'
            getpid_func = 'GetCurrentProcessId'
          setup_io_non_MSWin32:
            $P0 = dlfunc library, getpid_func, 'i'
            $I0 = 0
            unless $P0 goto setup_io_no_getpid_func
            $I0 = $P0()
          setup_io_no_getpid_func:
            $P0 = box $I0
            set_hll_global ['PROCESS'], '$PID', $P0
            ##  Parrot request: http://trac.parrot.org/parrot/ticket/1564 
        }
    }
}
