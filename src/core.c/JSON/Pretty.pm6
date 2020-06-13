sub to-json(|c) is implementation-detail {
    Rakudo::Deprecations.DEPRECATED(
      'JSON::Fast, JSON::Tiny or JSON::Pretty from https://modules.raku.org/'
    );
    Rakudo::Internals::JSON.to-json(|c);
}

sub from-json($text) is implementation-detail {
    Rakudo::Deprecations.DEPRECATED(
      'JSON::Fast, JSON::Tiny or JSON::Pretty from https://modules.raku.org/'
    );
    Rakudo::Internals::JSON.from-json($text);
}

# vim: expandtab shiftwidth=4
