# XXX Very cheaty, just to get us able to output something.
sub say(\$x) {
    my $list = ($x,).list;
    pir::print($list.shift) while $list.gimme(1);
    pir::print("\n");
}

sub print(\$x) {
    my $list = ($x,).list;
    pir::print($list.shift) while $list.gimme(1);
}

