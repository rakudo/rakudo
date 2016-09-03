sub to-json(|c) {
    DEPRECATED('JSON::Fast, JSON::Tiny or JSON::Pretty from https://modules.perl6.org/');
    Rakudo::Internals::JSON.to-json(|c);
}

sub from-json($text) {
    DEPRECATED('JSON::Fast, JSON::Tiny or JSON::Pretty from https://modules.perl6.org/');
    Rakudo::Internals::JSON.from-json($text);
}
