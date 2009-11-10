our multi infix:<~~>($topic, $matcher) {
    $matcher.ACCEPTS($topic)
}

our sub undefine($x is rw) {
    my $undefined;
    $x = $undefined;
}
