=begin pod

=head1 NAME

Safe - simplistic, crude Safe mode for Rakudo

=head1 Synopsis

    BEGIN { @*INC.push: 'lib' }
    use Safe;
    # rest of your code here, which can't use
    # run(), qx/../ or open()
    # (at least not easily)
    
=head1 Description

C<Safe> crudely disables the most dangerous commands in Rakudo, right now
C<run()>, C<qx/.../> and the C<open()> function (opening sockets is still
allowed, though). Don't rely on it now, embedded PIR might still do very nasty
things.

=end pod

module Safe {
    my $s = -> *@a, *%h { die "operation not permitted in safe mode" };
    Q:PIR {
        $P0 = get_hll_namespace
        $P1 = find_lex '$s'
        $P0['run']  = $P1
        $P0['open'] = $P1
        $P0['!qx']  = $P1
        null $P1
        set_hll_global ['IO'], 'Socket', $P0
    }
}

# vim: ft=perl6
