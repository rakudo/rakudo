sub to-json(|c) {
    Rakudo::Deprecations.DEPRECATED(
      'JSON::Fast, JSON::Tiny or JSON::Pretty from https://modules.perl6.org/'
    );
    Rakudo::Internals::JSON.to-json(|c);
}

sub from-json($text) {
    Rakudo::Deprecations.DEPRECATED(
      'JSON::Fast, JSON::Tiny or JSON::Pretty from https://modules.perl6.org/'
    );
    Rakudo::Internals::JSON.from-json($text);
}

# vim: ft=perl6 expandtab sw=4
