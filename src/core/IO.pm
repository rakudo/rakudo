# XXX Relatively cheaty, just to get us able to output something.
# But you should see what USED to be here! O.O
sub say(*@list) {
    pir::print(@list.shift) while @list.gimme(1);
    pir::print("\n");
    1.Bool
}

sub print(*@list) {
    pir::print(@list.shift) while @list.gimme(1);
    1.Bool
}
