# XXX Very cheaty, just to get us able to output something.
sub say(|$) {
    Q:PIR {
        $P0 = perl6_current_args_rpa
        $P1 = iter $P0
      loop:
        unless $P1 goto done
        $P2 = shift $P1
        print $P2
        goto loop
      done:
        print "\n"
    };
    1.Bool
}

sub print(|$) {
    Q:PIR {
        $P0 = perl6_current_args_rpa
        $P1 = iter $P0
      loop:
        unless $P1 goto done
        $P2 = shift $P1
        print $P2
        goto loop
      done:
    };
    1.Bool
}
