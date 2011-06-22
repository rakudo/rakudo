# XXX Relatively cheaty, just to get us able to output something.
# But you should see what USED to be here! O.O
sub print(*@list) {
    nqp::print(@list.shift) while @list.gimme(1);
    1.Bool
}

sub say(|$) {
    print pir__perl6_box_rpa__PP(pir::perl6_current_args_rpa__P()).gist, "\n"
}

sub gist(|$) {
    pir__perl6_box_rpa__PP(pir::perl6_current_args_rpa__P()).gist
}
