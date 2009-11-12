our multi infix:<~~>($topic, $matcher) {
    $matcher.ACCEPTS($topic)
}

our multi prefix:<not>($x) { !$x }

our multi prefix:<true>($x) { ?$x }

our sub undefine($x is ref) {
    my $undefined;
    $x = $undefined;
}
