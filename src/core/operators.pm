our multi infix:<~~>($topic, $matcher) {
    $matcher.ACCEPTS($topic)
}

our sub undefine($x is ref) {
    my $undefined;
    $x = $undefined;
}
