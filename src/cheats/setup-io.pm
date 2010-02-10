# Really we should be able to write $PROCESS::IN := ...

INIT {
    my $IN = IO.new(:PIO(pir::getstdin__P()));
    my $OUT = IO.new(:PIO(pir::getstdout__P()));
    my $ERR = IO.new(:PIO(pir::getstderr__P()));
    Q:PIR {
        $P0 = find_lex '$IN'
        set_hll_global ['PROCESS'], '$IN', $P0
        $P0 = find_lex '$OUT'
        set_hll_global ['PROCESS'], "$OUT", $P0
        $P0 = find_lex '$ERR'
        set_hll_global ['PROCESS'], '$ERR', $P0

        ##  Parrot doesn't give us the PID for now. Well, this file *is*
        ##  in the cheats directory... :-)
        $P0 = box 0
        set_hll_global ['PROCESS'], '$PID', $P0

        ##  set up $*OS, $*OSVER $*EXECUTABLE_NAME
        .include 'sysinfo.pasm'
        .local string info
        info = sysinfo .SYSINFO_PARROT_OS
        $P0 = new ['Str']
        $P0 = info
        set_hll_global ['PROCESS'], '$OS', $P0
        info = sysinfo .SYSINFO_PARROT_OS_VERSION
        $P0 = new ['Str']
        $P0 = info
        set_hll_global ['PROCESS'], '$OSVER', $P0
    }
}
