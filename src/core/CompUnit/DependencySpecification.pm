class CompUnit::DependencySpecification {
    has Str:D $.short-name         is required;
    has Int:D $.source-line-number = 0;
    has Str:D $.from = 'Perl6';
    has $.version-matcher = True;
    has $.auth-matcher = True;
    has $.api-matcher = True;

    method Str(CompUnit::DependencySpecification:D:) {
        join '', $.short-name,
          ($.version-matcher//True) ~~ Bool ?? '' !! ":ver<$.version-matcher>",
          ($.auth-matcher   //True) ~~ Bool ?? '' !! ":auth<$.auth-matcher>",
          ($.api-matcher    //True) ~~ Bool ?? '' !! ":api<$.api-matcher>";
    }
}

# vim: ft=perl6 expandtab sw=4
