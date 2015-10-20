class CompUnit::DependencySpecification {
    has Str:D $.short-name is required;
    has $.version-matcher = True;
    has $.auth-matcher = True;
    has $.api-matcher = True;
}

# vim: ft=perl6 expandtab sw=4
