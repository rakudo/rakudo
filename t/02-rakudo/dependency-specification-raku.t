use Test;
use MONKEY-SEE-NO-EVAL;

plan 6;

# A precompiled module stores the dependency specifications of its `use`
# statements as their .raku, and EVALs that when its dependencies need to
# be resolved again, e.g. after the repository chain changed.

sub roundtrip(*%matchers) {
    EVAL CompUnit::DependencySpecification.new(:short-name<Foo>, |%matchers).raku
}

is-deeply roundtrip(:auth-matcher('')).auth-matcher, '',
  'an empty auth survives a roundtrip through .raku';
is-deeply roundtrip(:auth-matcher('zef:a b')).auth-matcher, 'zef:a b',
  'an auth with a space survives a roundtrip through .raku';
is-deeply roundtrip(:auth-matcher('zef:a>b')).auth-matcher, 'zef:a>b',
  'an auth with a > survives a roundtrip through .raku';
is roundtrip(:version-matcher('1.2+')).version-matcher, v1.2+,
  'a version given as a string survives a roundtrip through .raku';
is roundtrip(:version-matcher(v1.2+)).version-matcher, v1.2+,
  'a version survives a roundtrip through .raku';
is roundtrip(:api-matcher('2')).api-matcher, v2,
  'an api survives a roundtrip through .raku';

# vim: expandtab shiftwidth=4
