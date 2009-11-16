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
    }
}
