# Really we should be able to write $PROCESS::IN := ...

INIT {
    Q:PIR {
        .local pmc IO
        IO = get_hll_global 'IO'

        $P0 = getstdin
        $P0.'encoding'('utf8')
        $P0 = IO.'new'('PIO'=>$P0)
        set_hll_global ['PROCESS'], '$IN', $P0

        $P0 = getstdout
        $P0.'encoding'('utf8')
        $P0 = IO.'new'('PIO'=>$P0)
        set_hll_global ['PROCESS'], '$OUT', $P0

        $P0 = getstderr
        $P0.'encoding'('utf8')
        $P0 = IO.'new'('PIO'=>$P0)
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
