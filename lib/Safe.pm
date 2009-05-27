module Safe {
    say "in module Safe";
    my $s = -> *@a, *%h { die "operation not permitted in safe mode" };
    Q:PIR {
        $P0 = get_hll_namespace
        $P1 = find_lex '$s'
        $P0['run']  = $P1
        $P0['open'] = $P1
        $P0['!qx']  = $P1
    }
}

# vim: ft=perl6
