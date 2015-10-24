class CompUnit::DependencySpecification {
    has Str:D $.short-name is required;
    has $.version-matcher = True;
    has $.auth-matcher = True;
    has $.api-matcher = True;

    method Str(CompUnit::DependencySpecification:D:) {
        return "{$.short-name}:ver<{$.version-matcher}>:auth<{$.auth-matcher}>:api<{$.api-matcher}>";
    }
}

# vim: ft=perl6 expandtab sw=4
