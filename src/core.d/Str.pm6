sub parse-names(Str:D \names) {
    Rakudo::Deprecations.DEPRECATED('uniparse', '6.d', '6.e');
    names.uniparse
}

